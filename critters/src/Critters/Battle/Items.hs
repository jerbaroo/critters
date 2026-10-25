module Critters.Battle.Items (escape, useItem, validateItem) where

import Control.Monad.Trans.State.Strict (State)
import Control.Monad.Trans.State.Strict qualified as State
import Critters.Battle.Common (Context (..), Side (..))
import Critters.Battle.Common qualified as Common
import Critters.Number (integer, number)
import Critters.State.Affliction qualified as Affliction
import Critters.State.Battle (Battle (..))
import Critters.State.Battle qualified as Battle
import Critters.State.BattleDefinition qualified as Definition
import Critters.State.BattleOutcome (BattleOutcome (..))
import Critters.State.BattleProfile qualified as Profile
import Critters.State.CombatConfig qualified as Config
import Critters.State.Critter (Critter)
import Critters.State.Critter qualified as Critter
import Critters.State.Item (Item)
import Critters.State.Item qualified as Item
import Critters.State.Move qualified as Move
import Critters.State.Party (Party (..))
import Critters.State.Rules qualified as Rules
import Critters.State.Species qualified as Species
import Data.Map.Strict qualified as Map
import Data.Text (Text)

escape :: State Context ()
escape = do
    context <- State.get
    let rules = Common.rulesFor context
        probability =
            Common.evaluate
                0
                rules.escapeFormula
                [ ("attempts", number context.battle.escapeAttempts)
                , ("enemyLevel", maybe 1 (number . (.level)) (Common.active Enemy context))
                , ("enemySpeed", Common.stat context Enemy rules.speedStat)
                , ("level", maybe 1 (number . (.level)) (Common.active Player context))
                , ("speed", Common.stat context Player rules.speedStat)
                ]
    escaped <- Common.chance probability
    if escaped
        then Common.endBattle Escaped
        else do
            State.modify $ \current -> current{Common.battle = current.battle{Battle.escapeAttempts = current.battle.escapeAttempts + 1}}
            Common.emit "The escape attempt failed."

restore :: Context -> Item -> Critter -> Critter
restore context item critter =
    critter
        { Critter.battleProfile = profile{Profile.remainingUses = resources, Profile.status = if item.cureStatus then Nothing else profile.status}
        , Critter.hitPoints = health
        }
  where
    health
        | critter.hitPoints == 0 = if item.reviveFraction > 0 then max 1 (integer (number critter.maxHitPoints * item.reviveFraction)) else 0
        | otherwise = min critter.maxHitPoints (integer (number critter.hitPoints + number item.healing + number critter.maxHitPoints * item.healthFraction))
    profile = critter.battleProfile
    resources = foldl' replenish profile.remainingUses (Common.knownMoves context.battle critter)
    replenish remaining identifier = case Map.lookup identifier context.battle.config.moves of
        Just move
            | move.maxUses > 0 && item.restoreUses > 0 ->
                let old = Common.moveRemaining critter identifier move
                    next = min move.maxUses (integer (number old + number item.restoreUses))
                 in if next == old then remaining else Map.insert identifier next remaining
        _ -> remaining

useItem :: Text -> Int -> State Context ()
useItem identifier target = do
    context <- State.get
    case validateItem context identifier target of
        Left message -> Common.emit message
        Right item -> do
            State.modify $ \current -> current{Common.inventory = Map.adjust (subtract 1) identifier current.inventory}
            if item.captureBonus > 0
                then capture item
                else case drop target context.party.members of
                    critter : _ -> do
                        State.modify (Common.setCritter Player target (restore context item critter))
                        Common.emit ("Used " <> (if item.name == "" then identifier else item.name) <> " on " <> critter.species <> ".")
                    [] -> pure ()
  where
    capture item = do
        context <- State.get
        case Common.active Enemy context of
            Nothing -> pure ()
            Just critter -> do
                let config = context.battle.config
                    rate = maybe 0 (.captureRate) (Map.lookup critter.species config.species)
                    bonus = maybe 1 (\affliction -> Map.findWithDefault 1 affliction.status ((Common.rulesFor context).statusCaptureBonuses)) critter.battleProfile.status
                    probability =
                        Common.evaluate
                            0
                            ((Common.rulesFor context).captureFormula)
                            [ ("hp", number critter.hitPoints)
                            , ("itemBonus", item.captureBonus)
                            , ("level", maybe 1 (number . (.level)) (Common.active Player context))
                            , ("maxHP", number critter.maxHitPoints)
                            , ("rate", rate)
                            , ("statusBonus", bonus)
                            , ("targetLevel", number critter.level)
                            ]
                captured <- Common.chance probability
                if not captured
                    then Common.emit "The capture attempt failed."
                    else do
                        State.modify $ \current ->
                            let updated = Common.setCritter Enemy (Common.activeIndex Enemy current) critter{Critter.hitPoints = 0} current
                             in updated{Common.party = Party (current.party.members <> [critter]), Common.battle = updated.battle{Battle.capturedOpponents = current.battle.capturedOpponents <> [Common.activeIndex Enemy current]}}
                        Common.emit ("Captured " <> critter.species <> ".")
                        remaining <- State.gets (any Common.usable . Common.allCritters Enemy)
                        if remaining then pure () else Common.endBattle Captured

validateItem :: Context -> Text -> Int -> Either Text Item
validateItem context identifier target = do
    item <- maybe (Left "That item is not defined.") Right (Map.lookup identifier context.battle.config.items)
    if Map.findWithDefault 0 identifier context.inventory <= 0 then Left "You have none of that item." else pure ()
    if item.captureBonus > 0
        then do
            if not context.battle.definition.allowCapture then Left "Capture is not permitted in this battle." else pure ()
            if length context.party.members >= context.limit then Left "Your party is full." else pure ()
            if maybe False Common.usable (Common.active Enemy context) then Right item else Left "There is no opponent to capture."
        else
            if target < 0
                then Left "Choose a party member."
                else case drop target context.party.members of
                    critter : _ | restore context item critter /= critter -> Right item
                    _ -> Left "That item would have no effect."
