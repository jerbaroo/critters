module Critters.Battle.Turn (accuracyFor, damageFor, resolveAction, settle) where

import Control.Monad (foldM, when)
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
import Critters.Event qualified as Event
import Critters.Message qualified as Message
import Critters.Number (number)
import Critters.State.Battle (Battle (..))
import Critters.State.Battle qualified as Battle
import Critters.State.BattleDefinition qualified as Definition
import Critters.State.BattleOutcome (BattleOutcome (..))
import Critters.State.BattlePhase (BattlePhase (..))
import Critters.State.BattleProfile qualified as Profile
import Critters.State.BattleSide qualified as BattleSide
import Critters.State.BattleUpdate (BattleUpdate (BattleUpdate))
import Critters.State.Critter qualified as Critter
import Critters.State.GameState (GameState)
import Critters.State.Message (BattleMessage (..), Presentation (..))
import Critters.State.Move qualified as Move
import Critters.State.MoveId (MoveId)
import Critters.State.Rules qualified as Rules
import Critters.State.Stage (StageValues (..))
import Data.List (findIndex, nub)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)

actionPriority :: Context -> Side -> Action -> Int
actionPriority context side = \case
    EscapeBattle -> rules.escapePriority
    SwitchCritter _ -> rules.switchPriority
    UseItem _ _ -> rules.itemPriority
    UseMove identifier -> maybe 0 (.priority) (lookup identifier (Common.availableMoves side context))
  where
    rules = Common.rulesFor context

executeAction :: Side -> Action -> State Context (Maybe MoveId)
executeAction side = \case
    EscapeBattle -> Items.escape >> pure Nothing
    SwitchCritter index -> switch side index >> pure Nothing
    UseItem identifier target -> Items.useItem identifier target >> pure Nothing
    UseMove identifier -> do
        allowed <- Status.beforeAction side
        performed <- if allowed then performMove side identifier else pure False
        pure (if performed then Just identifier else Nothing)

finish :: BattleOutcome -> State Context ()
finish = Common.endBattle

performMove :: Side -> MoveId -> State Context Bool
performMove side identifier = do
    context <- State.get
    case (Common.active side context, lookup identifier (Common.availableMoves side context)) of
        (Just attacker, Just move) -> do
            Common.emitKey "battleMoveUsed" [("species", Common.critterName attacker), ("move", Message.reference move.name)]
            when (move.maxUses > 0) $ Common.modifyActive side $ \critter ->
                critter
                    { Critter.battleProfile = critter.battleProfile{Profile.remainingUses = Map.insert identifier (max 0 (Common.moveRemaining critter identifier move - 1)) critter.battleProfile.remainingUses}
                    }
            connected <- Common.chance (accuracyFor context side move)
            if not connected
                then Common.emitKey "battleMoveMissed" []
                else do
                    hits <- if Map.null move.hitWeights then Common.chooseInt move.minHits move.maxHits else maybe move.minHits id <$> Common.chooseWeighted (Map.toAscList move.hitWeights)
                    sharedCritical <- if (Common.rulesFor context).criticalPerHit then pure False else criticalRoll move
                    damage <- strike move hits sharedCritical True
                    mapM_ (Status.applyEffect side damage) move.effects
            pure True
        _ -> pure False
  where
    criticalRoll move = do
        context <- State.get
        let level participant = maybe 1 (number . (.level)) (Common.active participant context)
            probability = Common.evaluate ((Common.rulesFor context).criticalFormula) [("criticalRate", move.criticalRate), ("speed", Common.stat context side ((Common.rulesFor context).speedStat)), ("level", level side), ("targetLevel", level (Common.opposite side))]
        Common.chance probability
    strike move remaining sharedCritical firstHit
        | remaining <= 0 = pure 0
        | otherwise = do
            context <- State.get
            case Common.active (Common.opposite side) context of
                Just defender | Common.usable defender -> do
                    connected <- if not firstHit && (Common.rulesFor context).accuracyPerHit then Common.chance (accuracyFor context side move) else pure True
                    if not connected
                        then Common.emitKey "battleMoveMissed" [] >> pure 0
                        else do
                            critical <- if (Common.rulesFor context).criticalPerHit then criticalRoll move else pure sharedCritical
                            let rules = Common.rulesFor context
                                multiplier = if critical then max 0 (Common.evaluate rules.criticalMultiplierFormula [("level", maybe 1 (number . (.level)) (Common.active side context)), ("targetLevel", number defender.level)]) else 1
                            variance <- if rules.randomDamageMin == rules.randomDamageMax then pure rules.randomDamageMin else (\roll -> rules.randomDamageMin + roll * (rules.randomDamageMax - rules.randomDamageMin)) <$> Common.randomUnit
                            let amount = min defender.hitPoints (damageFor context side move multiplier variance)
                            Common.modifyActive (Common.opposite side) (\critter -> critter{Critter.hitPoints = max 0 (critter.hitPoints - amount)})
                            when critical (Common.emitKey "battleCriticalHit" [])
                            when (amount > 0) (Common.emitDamage (Message.named "battleDamage" [("species", Common.critterName defender), ("amount", Message.numeric amount)]))
                            rest <- strike move (remaining - 1) sharedCritical False
                            pure (amount + rest)
                _ -> pure 0

recordAction :: Context -> Side -> Action -> State Context [Event]
recordAction before side action = do
    used <- executeAction side action
    after <- State.get
    let event = case used of
            Just identifier -> Event.MoveUsed (case side of Enemy -> BattleSide.Enemy; Player -> BattleSide.Player) identifier
            Nothing -> Event.BattleChanged
        facts = [event (updateBetween before after) | isJust used || before /= after]
    settle
    settled <- State.get
    pure (facts <> [Event.BattleChanged (updateBetween after settled) | after /= settled])

resolveAction :: GameState -> Action -> [Event]
resolveAction state action = case Common.contextFor state of
    Just context | context.battle.phase == Introduction -> []
    Just context | validAction context action -> State.evalState (runTurn action) context
    Just context | not (isJust context.battle.outcome) && context.battle.phase /= ReplacementMenu -> case action of
        UseItem identifier target -> case Items.validateItem context identifier target of
            Left message -> Common.changedEvents state context{Common.battle = context.battle{Battle.messages = [BattleMessage message Dialogue]}}
            Right _ -> []
        EscapeBattle -> Common.changedEvents state context{Common.battle = context.battle{Battle.messages = [BattleMessage (Message.named "battleEscapeForbidden" []) Dialogue]}}
        _ -> []
    _ -> []

runTurn :: Action -> State Context [Event]
runTurn action = do
    initial <- State.get
    State.modify $ \context -> context{Common.battle = context.battle{Battle.messages = [], Battle.phase = ActionMenu, Battle.selected = 0, Battle.selectedItem = Nothing}}
    if initial.battle.phase == ReplacementMenu
        then recordAction initial Player action
        else do
            enemyAction <- Opponent.chooseAction
            let playerPriority = actionPriority initial Player action
                enemyPriority = actionPriority initial Enemy enemyAction
                speed participant = Common.stat initial participant ((Common.rulesFor initial).speedStat)
            playerFirst <-
                if playerPriority /= enemyPriority
                    then pure (playerPriority > enemyPriority)
                    else
                        if speed Player /= speed Enemy
                            then pure (speed Player > speed Enemy)
                            else case (Common.rulesFor initial).tieBreak of
                                "opponent" -> pure False
                                "random" -> Common.chance 0.5
                                _ -> pure True
            let ordered = if playerFirst then [(Player, action), (Enemy, enemyAction)] else [(Enemy, enemyAction), (Player, action)]
            (recorded, facts) <- foldM (resolve initial) (initial, []) ordered
            Status.afterTurn initial
            settle
            State.modify $ \context -> context{Common.battle = context.battle{Battle.turn = context.battle.turn + 1}}
            final <- State.get
            pure (facts <> [Event.TurnCompleted (updateBetween recorded final)])
  where
    resolve initial (recorded, facts) (side, selectedAction) = do
        current <- State.get
        if not (isJust current.battle.outcome) && current.battle.phase /= ReplacementMenu && Common.activeIndex side current == Common.activeIndex side initial && maybe False Common.usable (Common.active side current)
            then do
                next <- recordAction recorded side selectedAction
                settled <- State.get
                pure (settled, facts <> next)
            else pure (recorded, facts)

settle :: State Context ()
settle = do
    Progression.rewardDefeated
    context <- State.get
    when (not (isJust context.battle.outcome)) $
        if not (any Common.usable (Common.allCritters Player context))
            then finish (if not (any Common.usable (Common.allCritters Enemy context)) && (Common.rulesFor context).simultaneousDefeatWins then Victory else Defeat)
            else
                if not (any Common.usable (Common.allCritters Enemy context))
                    then finish Victory
                    else do
                        when (maybe True (not . Common.usable) (Common.active Enemy context)) $
                            mapM_ (switch Enemy) (findIndex Common.usable (Common.allCritters Enemy context))
                        when (maybe True (not . Common.usable) (Common.active Player context)) $ do
                            State.modify $ \current -> current{Common.battle = current.battle{Battle.phase = ReplacementMenu, Battle.selected = 0}}
                            Common.emitKey "battleChooseReplacement" []

switch :: Side -> Int -> State Context ()
switch side index = do
    State.modify $ \context -> context{Common.battle = update ((Common.rulesFor context).resetStagesOnSwitch) context.battle}
    context <- State.get
    mapM_ (\critter -> Common.emitKey "battleEntered" [("species", Common.critterName critter)]) (Common.active side context)
  where
    update reset current
        | side == Player = current{Battle.activePlayer = index, Battle.participants = nub (current.participants <> [index]), Battle.phase = ActionMenu, Battle.playerStages = if reset then StageValues Map.empty else current.playerStages}
        | otherwise = current{Battle.activeOpponent = index, Battle.participants = [current.activePlayer], Battle.enemyStages = if reset then StageValues Map.empty else current.enemyStages}

updateBetween :: Context -> Context -> BattleUpdate
updateBetween before after =
    BattleUpdate
        after.battle
        (changed before.inventory after.inventory)
        (changed before.party after.party)
        (changed before.seed after.seed)
  where
    changed old new = if old == new then Nothing else Just new

validAction :: Context -> Action -> Bool
validAction context action
    | isJust context.battle.outcome = False
    | context.battle.phase == ReplacementMenu = case action of
        SwitchCritter index -> validSwitch index
        _ -> False
    | otherwise = case action of
        EscapeBattle -> context.battle.definition.allowEscape
        SwitchCritter index -> validSwitch index
        UseItem identifier target -> either (const False) (const True) (Items.validateItem context identifier target)
        UseMove identifier -> isJust (lookup identifier (Common.availableMoves Player context))
  where
    validSwitch index = index >= 0 && index /= Common.activeIndex Player context && maybe False Common.usable (case drop index (Common.allCritters Player context) of value : _ -> Just value; [] -> Nothing)
