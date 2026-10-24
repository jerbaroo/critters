module Critters.Battle.Status (afterTurn, applyEffect, beforeAction) where

import Control.Monad (forM_, when)
import Control.Monad.Trans.State.Strict (State)
import Control.Monad.Trans.State.Strict qualified as State
import Critters.Battle.Common (Context (..), Side (..))
import Critters.Battle.Common qualified as Common
import Critters.Number (integer, number)
import Critters.State.Affliction (Affliction (..))
import Critters.State.Battle (Battle (..))
import Critters.State.BattleProfile qualified as Profile
import Critters.State.CombatConfig qualified as Config
import Critters.State.Critter qualified as Critter
import Critters.State.MoveEffect (MoveEffect (..), Target (..))
import Critters.State.Species qualified as Species
import Critters.State.StatusDefinition qualified as Status
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Witch (from)

afterTurn :: Context -> State Context ()
afterTurn initial = forM_ [Enemy, Player] $ \side -> do
  context <- State.get
  case Common.active side context of
    Just critter | Common.usable critter && not (isJust (battleOutcome (contextBattle context))) ->
      case Profile.profileStatus (Critter.battleProfile critter) of
        Nothing -> pure ()
        Just affliction -> forM_ (Map.lookup (afflictionStatus affliction) (Config.configStatuses (battleConfig (contextBattle context)))) $ \definition -> do
          let previous = case drop (Common.activeIndex side context) (Common.allCritters side initial) of
                old : _ -> afflictionStatus <$> Profile.profileStatus (Critter.battleProfile old)
                [] -> Nothing
          when (Status.statusTickOnApply definition || previous == Just (afflictionStatus affliction)) $ do
            let amount = if Status.statusDamageFraction definition <= 0 then 0 else min (Critter.hitPoints critter) (max 1 (integer (number (Critter.maxHitPoints critter) * Status.statusDamageFraction definition)))
            when (amount > 0) $ do
              Common.modifyActive side (\value -> value{Critter.hitPoints = max 0 (Critter.hitPoints value - amount)})
              Common.emit (Critter.species critter <> " lost " <> from (show amount) <> " HP to " <> afflictionStatus affliction <> ".")
            case afflictionRemaining affliction of
              Just 1 -> do
                Common.modifyActive side (\value -> value{Critter.battleProfile = (Critter.battleProfile value){Profile.profileStatus = Nothing}})
                Common.emit (Critter.species critter <> " recovered from " <> afflictionStatus affliction <> ".")
              Just remaining -> Common.modifyActive side (\value -> value{Critter.battleProfile = (Critter.battleProfile value){Profile.profileStatus = Just affliction{afflictionRemaining = Just (remaining - 1)}}})
              Nothing -> pure ()
    _ -> pure ()

applyEffect :: Side -> Int -> MoveEffect -> State Context ()
applyEffect actor damage effect = do
  context <- State.get
  case effect of
    Chance probability nested -> Common.chance probability >>= \applies -> when applies (applyEffect actor damage nested)
    ChangeHealth target formula -> forM_ (Common.active (destination target) context) $ \critter -> when (Common.usable critter) $ do
      let amount = integer (Common.evaluate 0 formula [("damage", number damage), ("hp", number (Critter.hitPoints critter)), ("maxHP", number (Critter.maxHitPoints critter)), ("level", maybe 1 (number . Critter.level) (Common.active actor context)), ("targetHP", number (Critter.hitPoints critter)), ("targetMaxHP", number (Critter.maxHitPoints critter))])
          next = max 0 (min (Critter.maxHitPoints critter) (integer (number (Critter.hitPoints critter) + number amount)))
      Common.modifyActive (destination target) (\value -> value{Critter.hitPoints = next})
      when (next /= Critter.hitPoints critter) (Common.emit (Critter.species critter <> " HP changed by " <> from (show (next - Critter.hitPoints critter)) <> "."))
    ChangeStage target identifier amount -> when (maybe False Common.usable (Common.active (destination target) context)) $ do
      State.modify (Common.setStage (destination target) identifier amount)
      Common.emit (identifier <> " stage changed.")
    ClearStatus target -> do
      Common.modifyActive (destination target) (\value -> value{Critter.battleProfile = (Critter.battleProfile value){Profile.profileStatus = Nothing}})
      Common.emit "Status cleared."
    InflictStatus target identifier -> case (Common.active (destination target) context, Map.lookup identifier (Config.configStatuses (battleConfig (contextBattle context)))) of
      (Just critter, Just definition) | Common.usable critter && not (isJust (Profile.profileStatus (Critter.battleProfile critter))) -> do
        let affinities = maybe [] Species.speciesAffinities (Map.lookup (Critter.species critter) (Config.configSpecies (battleConfig (contextBattle context))))
        if any (`elem` Status.statusImmuneAffinities definition) affinities then Common.emit "The status had no effect."
        else do
          duration <- if Status.statusDurationMax definition == 0 then pure Nothing else Just <$> Common.chooseInt (Status.statusDurationMin definition) (Status.statusDurationMax definition)
          Common.modifyActive (destination target) (\value -> value{Critter.battleProfile = (Critter.battleProfile value){Profile.profileStatus = Just (Affliction duration identifier)}})
          Common.emit (Critter.species critter <> " is affected by " <> identifier <> ".")
      _ -> pure ()
  where
    destination Self = actor
    destination Opponent = Common.opposite actor

beforeAction :: Side -> State Context Bool
beforeAction side = do
  context <- State.get
  case Common.active side context >>= Profile.profileStatus . Critter.battleProfile of
    Nothing -> pure True
    Just affliction -> case Map.lookup (afflictionStatus affliction) (Config.configStatuses (battleConfig (contextBattle context))) of
      Nothing -> pure True
      Just definition -> do
        cured <- Common.chance (Status.statusCureChance definition)
        if cured then do
          Common.modifyActive side (\critter -> critter{Critter.battleProfile = (Critter.battleProfile critter){Profile.profileStatus = Nothing}})
          Common.emit ("Recovered from " <> afflictionStatus affliction <> ".")
          pure True
        else do
          blocked <- Common.chance (Status.statusBlockChance definition)
          when blocked (Common.emit ("Unable to act because of " <> afflictionStatus affliction <> "."))
          pure (not blocked)
