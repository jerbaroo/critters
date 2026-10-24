module Critters.Battle.Turn (accuracyFor, damageFor, resolveAction, settle) where

import Control.Monad (forM_, when)
import Control.Monad.Trans.State.Strict (State)
import Control.Monad.Trans.State.Strict qualified as State
import Critters.Battle.Action (Action (..))
import Critters.Battle.Common (Context (..), Side (..))
import Critters.Battle.Common qualified as Common
import Critters.Battle.Damage (accuracyFor, damageFor)
import Critters.Battle.Items qualified as Items
import Critters.Battle.Opponent qualified as Opponent
import Critters.Battle.Progression qualified as Progression
import Critters.Battle.Status qualified as Status
import Critters.Event (Event)
import Critters.Number (number)
import Critters.State.Battle (Battle (..))
import Critters.State.BattleDefinition qualified as Definition
import Critters.State.BattleOutcome (BattleOutcome (..))
import Critters.State.BattlePhase (BattlePhase (..))
import Critters.State.BattleProfile qualified as Profile
import Critters.State.Critter qualified as Critter
import Critters.State.GameState (GameState)
import Critters.State.Move qualified as Move
import Critters.State.MoveId (MoveId)
import Critters.State.Rules qualified as Rules
import Data.List (findIndex, nub)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Witch (from)

actionPriority :: Context -> Side -> Action -> Int
actionPriority context side = \case
  EscapeBattle -> Rules.rulesEscapePriority rules
  SwitchCritter _ -> Rules.rulesSwitchPriority rules
  UseItem _ _ -> Rules.rulesItemPriority rules
  UseMove identifier -> maybe 0 Move.movePriority (lookup identifier (Common.availableMoves side context))
  where
    rules = Common.rulesFor context

executeAction :: Side -> Action -> State Context ()
executeAction side = \case
  EscapeBattle -> Items.escape
  SwitchCritter index -> switch side index
  UseItem identifier target -> Items.useItem identifier target
  UseMove identifier -> Status.beforeAction side >>= \allowed -> when allowed (performMove side identifier)

finish :: BattleOutcome -> State Context ()
finish = Common.endBattle

performMove :: Side -> MoveId -> State Context ()
performMove side identifier = do
  context <- State.get
  case (Common.active side context, lookup identifier (Common.availableMoves side context)) of
    (Just attacker, Just move) -> do
      Common.emit (Critter.species attacker <> " used " <> Move.moveName move <> ".")
      when (Move.moveMaxUses move > 0) $ Common.modifyActive side $ \critter ->
        critter
          { Critter.battleProfile = (Critter.battleProfile critter){Profile.profileRemainingUses = Map.insert identifier (max 0 (Common.moveRemaining critter identifier move - 1)) (Profile.profileRemainingUses (Critter.battleProfile critter))}
          }
      connected <- Common.chance (accuracyFor context side move)
      if not connected
        then Common.emit "The move missed."
        else do
          hits <- if Map.null (Move.moveHitWeights move) then Common.chooseInt (Move.moveMinHits move) (Move.moveMaxHits move) else maybe (Move.moveMinHits move) id <$> Common.chooseWeighted (Map.toAscList (Move.moveHitWeights move))
          sharedCritical <- if Rules.rulesCriticalPerHit (Common.rulesFor context) then pure False else criticalRoll move
          damage <- strike move hits sharedCritical True
          mapM_ (Status.applyEffect side damage) (Move.moveEffects move)
    _ -> pure ()
  where
    criticalRoll move = do
      context <- State.get
      let level participant = maybe 1 (number . Critter.level) (Common.active participant context)
          probability = Common.evaluate 0 (Rules.rulesCriticalFormula (Common.rulesFor context)) [("criticalRate", Move.moveCriticalRate move), ("speed", Common.stat context side (Rules.rulesSpeedStat (Common.rulesFor context))), ("level", level side), ("targetLevel", level (Common.opposite side))]
      Common.chance probability
    strike move remaining sharedCritical firstHit
      | remaining <= 0 = pure 0
      | otherwise = do
          context <- State.get
          case Common.active (Common.opposite side) context of
            Just defender | Common.usable defender -> do
              connected <- if not firstHit && Rules.rulesAccuracyPerHit (Common.rulesFor context) then Common.chance (accuracyFor context side move) else pure True
              if not connected
                then Common.emit "The move missed." >> pure 0
                else do
                  critical <- if Rules.rulesCriticalPerHit (Common.rulesFor context) then criticalRoll move else pure sharedCritical
                  let rules = Common.rulesFor context
                      multiplier = if critical then max 0 (Common.evaluate 1 (Rules.rulesCriticalMultiplierFormula rules) [("level", maybe 1 (number . Critter.level) (Common.active side context)), ("targetLevel", number (Critter.level defender))]) else 1
                  variance <- if Rules.rulesRandomDamageMin rules == Rules.rulesRandomDamageMax rules then pure (Rules.rulesRandomDamageMin rules) else (\roll -> Rules.rulesRandomDamageMin rules + roll * (Rules.rulesRandomDamageMax rules - Rules.rulesRandomDamageMin rules)) <$> Common.randomUnit
                  let amount = min (Critter.hitPoints defender) (damageFor context side move multiplier variance)
                  Common.modifyActive (Common.opposite side) (\critter -> critter{Critter.hitPoints = max 0 (Critter.hitPoints critter - amount)})
                  when critical (Common.emit "Critical hit.")
                  when (amount > 0) (Common.emit (Critter.species defender <> " lost " <> from (show amount) <> " HP."))
                  rest <- strike move (remaining - 1) sharedCritical False
                  pure (amount + rest)
            _ -> pure 0

resolveAction :: GameState -> Action -> [Event]
resolveAction state action = case Common.contextFor state of
  Just context | validAction context action -> Common.changedEvents state (State.execState (runTurn action) context)
  Just context | not (isJust (battleOutcome (contextBattle context))) && battlePhase (contextBattle context) /= ReplacementMenu -> case action of
    UseItem identifier target -> case Items.validateItem context identifier target of
      Left message -> Common.changedEvents state context{contextBattle = (contextBattle context){battleMessages = [message]}}
      Right _ -> []
    EscapeBattle -> Common.changedEvents state context{contextBattle = (contextBattle context){battleMessages = ["Escape is not permitted in this battle."]}}
    _ -> []
  _ -> []

runTurn :: Action -> State Context ()
runTurn action = do
  initial <- State.get
  State.modify $ \context -> context{contextBattle = (contextBattle context){battleMessages = [], battlePhase = ActionMenu, battleSelected = 0, battleSelectedItem = Nothing}}
  if battlePhase (contextBattle initial) == ReplacementMenu
    then executeAction Player action >> settle
    else do
      enemyAction <- Opponent.chooseAction
      let playerPriority = actionPriority initial Player action
          enemyPriority = actionPriority initial Enemy enemyAction
          speed participant = Common.stat initial participant (Rules.rulesSpeedStat (Common.rulesFor initial))
      playerFirst <-
        if playerPriority /= enemyPriority
          then pure (playerPriority > enemyPriority)
          else
            if speed Player /= speed Enemy
              then pure (speed Player > speed Enemy)
              else case Rules.rulesTieBreak (Common.rulesFor initial) of
                "opponent" -> pure False
                "random" -> Common.chance 0.5
                _ -> pure True
      let ordered = if playerFirst then [(Player, action), (Enemy, enemyAction)] else [(Enemy, enemyAction), (Player, action)]
      forM_ ordered $ \(side, selectedAction) -> do
        current <- State.get
        when (not (isJust (battleOutcome (contextBattle current))) && battlePhase (contextBattle current) /= ReplacementMenu && Common.activeIndex side current == Common.activeIndex side initial && maybe False Common.usable (Common.active side current)) $ executeAction side selectedAction >> settle
      Status.afterTurn initial
      settle
      State.modify $ \context -> context{contextBattle = (contextBattle context){battleTurn = battleTurn (contextBattle context) + 1}}

settle :: State Context ()
settle = do
  Progression.rewardDefeated
  context <- State.get
  when (not (isJust (battleOutcome (contextBattle context)))) $
    if not (any Common.usable (Common.allCritters Player context))
      then finish (if not (any Common.usable (Common.allCritters Enemy context)) && Rules.rulesSimultaneousDefeatWins (Common.rulesFor context) then Victory else Defeat)
      else
        if not (any Common.usable (Common.allCritters Enemy context))
          then finish Victory
          else do
            when (maybe True (not . Common.usable) (Common.active Enemy context)) $
              mapM_ (switch Enemy) (findIndex Common.usable (Common.allCritters Enemy context))
            when (maybe True (not . Common.usable) (Common.active Player context)) $ do
              State.modify $ \current -> current{contextBattle = (contextBattle current){battlePhase = ReplacementMenu, battleSelected = 0}}
              Common.emit "Choose another critter."

switch :: Side -> Int -> State Context ()
switch side index = do
  State.modify $ \context -> context{contextBattle = update (Rules.rulesResetStagesOnSwitch (Common.rulesFor context)) (contextBattle context)}
  context <- State.get
  mapM_ (\critter -> Common.emit (Critter.species critter <> " entered the battle.")) (Common.active side context)
  where
    update reset current
      | side == Player = current{battleActivePlayer = index, battleParticipants = nub (battleParticipants current <> [index]), battlePhase = ActionMenu, battlePlayerStages = if reset then Map.empty else battlePlayerStages current}
      | otherwise = current{battleActiveOpponent = index, battleParticipants = [battleActivePlayer current], battleEnemyStages = if reset then Map.empty else battleEnemyStages current}

validAction :: Context -> Action -> Bool
validAction context action
  | isJust (battleOutcome (contextBattle context)) = False
  | battlePhase (contextBattle context) == ReplacementMenu = case action of
      SwitchCritter index -> validSwitch index
      _ -> False
  | otherwise = case action of
      EscapeBattle -> Definition.definitionAllowEscape (battleDefinition (contextBattle context))
      SwitchCritter index -> validSwitch index
      UseItem identifier target -> either (const False) (const True) (Items.validateItem context identifier target)
      UseMove identifier -> isJust (lookup identifier (Common.availableMoves Player context))
  where
    validSwitch index = index >= 0 && index /= Common.activeIndex Player context && maybe False Common.usable (case drop index (Common.allCritters Player context) of value : _ -> Just value; [] -> Nothing)
