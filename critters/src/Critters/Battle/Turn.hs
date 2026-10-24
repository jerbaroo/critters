module Critters.Battle.Turn (damageFor, resolveAction, settle) where

import Control.Monad (forM_, when)
import Control.Monad.Trans.State.Strict (State)
import Control.Monad.Trans.State.Strict qualified as State
import Critters.Battle.Action (Action (..))
import Critters.Battle.Common (Context (..), Side (..))
import Critters.Battle.Common qualified as Common
import Critters.Event (Event)
import Critters.Number (integer, number)
import Critters.State.Battle (Battle (..))
import Critters.State.BattleOutcome (BattleOutcome (..))
import Critters.State.BattlePhase (BattlePhase (..))
import Critters.State.BattleProfile qualified as Profile
import Critters.State.CombatConfig qualified as Config
import Critters.State.Critter qualified as Critter
import Critters.State.GameState (GameState)
import Critters.State.Move (Move)
import Critters.State.Move qualified as Move
import Critters.State.MoveId (MoveId (..))
import Critters.State.Rules qualified as Rules
import Critters.State.Species qualified as Species
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

damageFor :: Context -> Side -> Move -> Double -> Double -> Int
damageFor context side move critical variance
  | Move.movePower move <= 0 || affinity == 0 = 0
  | otherwise = max (Rules.rulesMinimumDamage rules) $ integer $ Common.evaluate 0 (Rules.rulesDamageFormula rules)
      [ ("affinity", affinity), ("attack", attack), ("baseAttack", baseAttack), ("baseDefense", baseDefense),
        ("critical", critical), ("defense", defense), ("level", level side), ("power", number (Move.movePower move)),
        ("sameAffinity", sameAffinity), ("targetLevel", level (Common.opposite side)), ("variance", variance)
      ]
  where
    affinity = product [Map.findWithDefault 1 defending (Map.findWithDefault Map.empty (Move.moveAffinity move) (Rules.rulesAffinityChart rules)) | defending <- affinities (Common.opposite side)]
    affinities participant = maybe [] (maybe [] Species.speciesAffinities . (\critter -> Map.lookup (Critter.species critter) (Config.configSpecies (battleConfig (contextBattle context))))) (Common.active participant context)
    attack = if critical > 1 && Rules.rulesCriticalIgnoresStages rules then baseAttack else Common.stat context side (Move.moveAttackStat move)
    baseAttack = maybe 1 (Map.findWithDefault 1 (Move.moveAttackStat move) . Common.statsFor (contextBattle context)) (Common.active side context)
    baseDefense = maybe 1 (Map.findWithDefault 1 (Move.moveDefenseStat move) . Common.statsFor (contextBattle context)) (Common.active (Common.opposite side) context)
    defense = if critical > 1 && Rules.rulesCriticalIgnoresStages rules then baseDefense else Common.stat context (Common.opposite side) (Move.moveDefenseStat move)
    level participant = maybe 1 (number . Critter.level) (Common.active participant context)
    rules = Common.rulesFor context
    sameAffinity = if Move.moveAffinity move `elem` affinities side then Rules.rulesSameAffinityBonus rules else 1

executeAction :: Side -> Action -> State Context ()
executeAction side = \case
  EscapeBattle -> pure ()
  SwitchCritter index -> switch side index
  UseItem _ _ -> pure ()
  UseMove identifier -> performMove side identifier

finish :: BattleOutcome -> State Context ()
finish outcome = do
  State.modify $ \context -> context
    { contextBattle = (contextBattle context){battleOutcome = Just outcome, battlePhase = Finished, battleSelected = 0},
      contextParty = if outcome == Defeat then Common.healParty (Rules.rulesDefeatRecovery (Common.rulesFor context)) (contextParty context) else contextParty context
    }
  Common.emit $ case outcome of
    Captured -> "Capture complete."
    Defeat -> "Defeat."
    Escaped -> "Escaped safely."
    Victory -> "Victory."

performMove :: Side -> MoveId -> State Context ()
performMove side identifier = do
  context <- State.get
  case (Common.active side context, lookup identifier (Common.availableMoves side context)) of
    (Just attacker, Just move) -> do
      Common.emit (Critter.species attacker <> " used " <> Move.moveName move <> ".")
      when (Move.moveMaxUses move > 0) $ Common.modifyActive side $ \critter -> critter
        { Critter.battleProfile = (Critter.battleProfile critter){Profile.profileRemainingUses = Map.insert identifier (max 0 (Common.moveRemaining critter identifier move - 1)) (Profile.profileRemainingUses (Critter.battleProfile critter))}
        }
      case Common.active (Common.opposite side) context of
        Nothing -> pure ()
        Just defender -> do
          let amount = min (Critter.hitPoints defender) (damageFor context side move 1 1)
          Common.modifyActive (Common.opposite side) (\critter -> critter{Critter.hitPoints = max 0 (Critter.hitPoints critter - amount)})
          when (amount > 0) (Common.emit (Critter.species defender <> " lost " <> from (show amount) <> " HP."))
    _ -> pure ()

resolveAction :: GameState -> Action -> [Event]
resolveAction state action = case Common.contextFor state of
  Just context | validAction context action -> Common.changedEvents state (State.execState (runTurn action) context)
  _ -> []

runTurn :: Action -> State Context ()
runTurn action = do
  initial <- State.get
  State.modify $ \context -> context{contextBattle = (contextBattle context){battleMessages = [], battleSelected = 0}}
  if battlePhase (contextBattle initial) == ReplacementMenu
    then executeAction Player action >> settle
    else do
      let enemyAction = case Common.availableMoves Enemy initial of
            (identifier, _) : _ -> UseMove identifier
            [] -> UseMove (MoveId "")
          playerPriority = actionPriority initial Player action
          enemyPriority = actionPriority initial Enemy enemyAction
          speed participant = Common.stat initial participant (Rules.rulesSpeedStat (Common.rulesFor initial))
          playerFirst = playerPriority > enemyPriority || (playerPriority == enemyPriority && speed Player >= speed Enemy)
          ordered = if playerFirst then [(Player, action), (Enemy, enemyAction)] else [(Enemy, enemyAction), (Player, action)]
      forM_ ordered $ \(side, selectedAction) -> do
        current <- State.get
        when (not (isJust (battleOutcome (contextBattle current))) && battlePhase (contextBattle current) /= ReplacementMenu && Common.activeIndex side current == Common.activeIndex side initial && maybe False Common.usable (Common.active side current)) $ executeAction side selectedAction >> settle
      State.modify $ \context -> context{contextBattle = (contextBattle context){battleTurn = battleTurn (contextBattle context) + 1}}

settle :: State Context ()
settle = do
  context <- State.get
  when (not (isJust (battleOutcome (contextBattle context)))) $
    if not (any Common.usable (Common.allCritters Player context)) then finish Defeat
    else if not (any Common.usable (Common.allCritters Enemy context)) then finish Victory
    else do
      when (maybe True (not . Common.usable) (Common.active Enemy context)) $
        mapM_ (switch Enemy) (findIndex Common.usable (Common.allCritters Enemy context))
      when (maybe True (not . Common.usable) (Common.active Player context)) $ do
        State.modify $ \current -> current{contextBattle = (contextBattle current){battlePhase = ReplacementMenu, battleSelected = 0}}
        Common.emit "Choose another critter."

switch :: Side -> Int -> State Context ()
switch side index = do
  State.modify $ \context -> context{contextBattle = update (contextBattle context)}
  context <- State.get
  mapM_ (\critter -> Common.emit (Critter.species critter <> " entered the battle.")) (Common.active side context)
  where
    update current
      | side == Player = current{battleActivePlayer = index, battleParticipants = nub (battleParticipants current <> [index]), battlePhase = ActionMenu, battlePlayerStages = Map.empty}
      | otherwise = current{battleActiveOpponent = index, battleEnemyStages = Map.empty}

validAction :: Context -> Action -> Bool
validAction context action
  | isJust (battleOutcome (contextBattle context)) = False
  | battlePhase (contextBattle context) == ReplacementMenu = case action of
      SwitchCritter index -> validSwitch index
      _ -> False
  | otherwise = case action of
      EscapeBattle -> False
      SwitchCritter index -> validSwitch index
      UseItem _ _ -> False
      UseMove identifier -> isJust (lookup identifier (Common.availableMoves Player context))
  where
    validSwitch index = index >= 0 && index /= Common.activeIndex Player context && maybe False Common.usable (case drop index (Common.allCritters Player context) of value : _ -> Just value; [] -> Nothing)
