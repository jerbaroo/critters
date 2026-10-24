module Critters.State.CombatConfig (CombatConfig (..), defaultCombat) where

import Autodocodec qualified as Codec
import Autodocodec (Autodocodec (..), HasCodec)
import Critters.State.Affliction qualified as Affliction
import Critters.State.BattleDefinition (BattleDefinition)
import Critters.State.BattleDefinition qualified as Definition
import Critters.State.BattleProfile qualified as Profile
import Critters.State.Critter qualified as Critter
import Critters.State.Encounter (Encounter)
import Critters.State.Encounter qualified as Encounter
import Critters.State.EncounterEntry qualified as Entry
import Critters.State.Item (Item)
import Critters.State.Move (Move)
import Critters.State.Move qualified as Move
import Critters.State.MoveEffect (MoveEffect (..))
import Critters.State.MoveId (MoveId)
import Critters.State.Rules (Rules, defaultRules)
import Critters.State.Rules qualified as Rules
import Critters.State.Species (Species)
import Critters.State.Species qualified as Species
import Critters.State.StatusDefinition (StatusDefinition)
import Data.Aeson qualified as Aeson
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text

data CombatConfig = CombatConfig
  { configBattles :: Map Text BattleDefinition,
    configEncounters :: [Encounter],
    configItems :: Map Text Item,
    configMoves :: Map MoveId Move,
    configRules :: Rules,
    configSpecies :: Map Text Species,
    configStatuses :: Map Text StatusDefinition
  }
  deriving stock (Eq)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec CombatConfig)

defaultCombat :: CombatConfig
defaultCombat = CombatConfig
  { configBattles = Map.empty,
    configEncounters = [],
    configItems = Map.empty,
    configMoves = Map.empty,
    configRules = defaultRules,
    configSpecies = Map.empty,
    configStatuses = Map.empty
  }

instance HasCodec CombatConfig where
  codec = Codec.bimapCodec validate id $ Codec.object "CombatConfig" $ CombatConfig
    <$> Codec.optionalFieldWithOmittedDefault "battles" (Map.empty) "Named, scripted or encounter battles." Codec..= configBattles
    <*> Codec.optionalFieldWithOmittedDefault "encounters" ([]) "Encounter tables." Codec..= configEncounters
    <*> Codec.optionalFieldWithOmittedDefault "items" (Map.empty) "Battle item definitions." Codec..= configItems
    <*> Codec.optionalFieldWithOmittedDefault "moves" (Map.empty) "Move definitions." Codec..= configMoves
    <*> Codec.optionalFieldWithOmittedDefault "rules" (defaultRules) "Game-defined combat and progression rules." Codec..= configRules
    <*> Codec.optionalFieldWithOmittedDefault "species" (Map.empty) "Species stats, affinities and learnsets." Codec..= configSpecies
    <*> Codec.optionalFieldWithOmittedDefault "statuses" (Map.empty) "Persistent status definitions." Codec..= configStatuses
    where
      validate value
        | any (Text.null . Text.strip) (Map.keys (configBattles value) <> Map.keys (configItems value) <> Map.keys (configSpecies value) <> Map.keys (configStatuses value)) = Left "Combat identifiers must not be blank."
        | any (not . knownMove) (concatMap (concat . Map.elems . Species.speciesLearnset) (Map.elems (configSpecies value))) = Left "A species learnset references a missing move."
        | any (not . validEffect) (Move.moveEffects (Rules.rulesFallbackMove (configRules value)) <> concatMap Move.moveEffects (Map.elems (configMoves value))) = Left "A move effect references an unknown status or invalid stat."
        | any (not . validCritter) (concatMap Definition.definitionOpponents (Map.elems (configBattles value))) = Left "An opposing critter references an unknown species, move or status."
        | any (not . (`Map.member` configBattles value) . Entry.entryBattle) (concatMap Encounter.encounterEntries (configEncounters value)) = Left "An encounter references a missing battle."
        | any (\entry -> maybe False (> Rules.rulesMaxLevel (configRules value)) (Entry.entryMinLevel entry) || maybe False (> Rules.rulesMaxLevel (configRules value)) (Entry.entryMaxLevel entry)) (concatMap Encounter.encounterEntries (configEncounters value)) = Left "Encounter levels exceed the game level limit."
        | any (not . (`Map.member` configStatuses value)) (Map.keys (Rules.rulesStatusCaptureBonuses (configRules value))) = Left "A capture bonus references a missing status."
        | otherwise = Right value
        where
          knownMove identifier = Map.member identifier (configMoves value)
          validCritter critter = Map.member (Critter.species critter) (configSpecies value) && all knownMove (Profile.profileKnownMoves (Critter.battleProfile critter)) && maybe True (\affliction -> Map.member (Affliction.afflictionStatus affliction) (configStatuses value)) (Profile.profileStatus (Critter.battleProfile critter))
          validEffect = \case
            Chance _ effect -> validEffect effect
            ChangeHealth _ _ -> True
            ChangeStage _ stat _ -> not (Text.null (Text.strip stat))
            ClearStatus _ -> True
            InflictStatus _ identifier -> Map.member identifier (configStatuses value)
