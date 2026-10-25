module Critters.Battle.Status (afterTurn, applyEffect, beforeAction) where

import Control.Monad (forM_, when)
import Control.Monad.Trans.State.Strict (State)
import Control.Monad.Trans.State.Strict qualified as State
import Critters.Battle.Common (Context (..), Side (..))
import Critters.Battle.Common qualified as Common
import Critters.Battle.Message qualified as Message
import Critters.Message qualified as Template
import Critters.Number (integer, number)
import Critters.State.Affliction (Affliction (..))
import Critters.State.Affliction qualified as Affliction
import Critters.State.Battle (Battle (..))
import Critters.State.BattleProfile qualified as Profile
import Critters.State.CombatConfig qualified as Config
import Critters.State.Critter qualified as Critter
import Critters.State.MoveEffect (MoveEffect (..), Target (..))
import Critters.State.Species qualified as Species
import Critters.State.StatusDefinition qualified as Status
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)

afterTurn :: Context -> State Context ()
afterTurn initial = forM_ [Enemy, Player] $ \side -> do
    context <- State.get
    case Common.active side context of
        Just critter | Common.usable critter && not (isJust context.battle.outcome) ->
            case critter.battleProfile.status of
                Nothing -> pure ()
                Just affliction -> forM_ (Map.lookup affliction.status context.battle.config.statuses) $ \definition -> do
                    let previous = case drop (Common.activeIndex side context) (Common.allCritters side initial) of
                            old : _ -> (.status) <$> old.battleProfile.status
                            [] -> Nothing
                    when (definition.tickOnApply || previous == Just affliction.status) $ do
                        let amount = if definition.damageFraction <= 0 then 0 else min critter.hitPoints (max 1 (integer (number critter.maxHitPoints * definition.damageFraction)))
                        when (amount > 0) $ do
                            Common.modifyActive side (\value -> value{Critter.hitPoints = max 0 (value.hitPoints - amount)})
                            Common.emitKey "statusDamage" [("species", Common.critterName critter), ("amount", Template.numeric amount), ("status", Template.reference ("{combat.statuses." <> affliction.status <> ".name}"))]
                        case affliction.remaining of
                            Just 1 -> do
                                Common.modifyActive side (\value -> value{Critter.battleProfile = value.battleProfile{Profile.status = Nothing}})
                                Common.emitKey "statusRecovered" [("species", Common.critterName critter), ("status", Template.reference ("{combat.statuses." <> affliction.status <> ".name}"))]
                            Just remaining -> Common.modifyActive side (\value -> value{Critter.battleProfile = value.battleProfile{Profile.status = Just affliction{Affliction.remaining = Just (remaining - 1)}}})
                            Nothing -> pure ()
        _ -> pure ()

applyEffect :: Side -> Int -> MoveEffect -> State Context ()
applyEffect actor damage effect = do
    context <- State.get
    case effect of
        Chance probability nested -> Common.chance probability >>= \applies -> when applies (applyEffect actor damage nested)
        ChangeHealth target formula -> forM_ (Common.active (destination target) context) $ \critter -> when (Common.usable critter) $ do
            let amount = integer (Common.evaluate formula [("damage", number damage), ("hp", number critter.hitPoints), ("maxHP", number critter.maxHitPoints), ("level", maybe 1 (number . (.level)) (Common.active actor context)), ("targetHP", number critter.hitPoints), ("targetMaxHP", number critter.maxHitPoints)])
                next = max 0 (min critter.maxHitPoints (integer (number critter.hitPoints + number amount)))
            Common.modifyActive (destination target) (\value -> value{Critter.hitPoints = next})
            when (next /= critter.hitPoints) (Common.emitKey "healthChanged" [("species", Common.critterName critter), ("amount", Template.numeric (next - critter.hitPoints))])
        ChangeStage target identifier amount -> forM_ (Common.active (destination target) context) $ \critter -> when (Common.usable critter) $ do
            change <- State.state (Common.setStage (destination target) identifier amount)
            Common.emit (Message.stageMessage context.battle.config (destination target) critter.species change)
        ClearStatus target -> do
            Common.modifyActive (destination target) (\value -> value{Critter.battleProfile = value.battleProfile{Profile.status = Nothing}})
            Common.emitKey "statusCleared" []
        InflictStatus target identifier -> case (Common.active (destination target) context, Map.lookup identifier context.battle.config.statuses) of
            (Just critter, Just definition) | Common.usable critter && not (isJust critter.battleProfile.status) -> do
                let critterTypes = maybe [] (.types) (Map.lookup critter.species context.battle.config.species)
                if any (`elem` definition.immuneTypes) critterTypes
                    then Common.emitKey "statusNoEffect" []
                    else do
                        duration <- if definition.durationMax == 0 then pure Nothing else Just <$> Common.chooseInt definition.durationMin definition.durationMax
                        Common.modifyActive (destination target) (\value -> value{Critter.battleProfile = value.battleProfile{Profile.status = Just (Affliction duration identifier)}})
                        Common.emitKey "statusInflicted" [("species", Common.critterName critter), ("status", Template.reference ("{combat.statuses." <> identifier <> ".name}"))]
            _ -> pure ()
  where
    destination Self = actor
    destination Opponent = Common.opposite actor

beforeAction :: Side -> State Context Bool
beforeAction side = do
    context <- State.get
    case Common.active side context >>= (.battleProfile.status) of
        Nothing -> pure True
        Just affliction -> case Map.lookup affliction.status context.battle.config.statuses of
            Nothing -> pure True
            Just definition -> do
                cured <- Common.chance definition.cureChance
                if cured
                    then do
                        Common.modifyActive side (\critter -> critter{Critter.battleProfile = critter.battleProfile{Profile.status = Nothing}})
                        Common.emitKey "statusActionRecovered" [("status", Template.reference ("{combat.statuses." <> affliction.status <> ".name}"))]
                        pure True
                    else do
                        blocked <- Common.chance definition.blockChance
                        when blocked (Common.emitKey "statusActionBlocked" [("status", Template.reference ("{combat.statuses." <> affliction.status <> ".name}"))])
                        pure (not blocked)
