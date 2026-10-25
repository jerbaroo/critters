module Critters.State.CombatConfig (CombatConfig (..), defaultCombat) where

import Autodocodec (Autodocodec (..), HasCodec)
import Autodocodec qualified as Codec
import Critters.Expression qualified as Expression
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
import Critters.State.MoveId (MoveId (..))
import Critters.State.Rules (Rules, defaultRules)
import Critters.State.Rules qualified as Rules
import Critters.State.Species (Species)
import Critters.State.Species qualified as Species
import Critters.State.Stage (StageDefinitions (..))
import Critters.State.StatName (StatName)
import Critters.State.StatusDefinition (StatusDefinition)
import Data.Aeson qualified as Aeson
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text

data CombatConfig = CombatConfig
    { battles :: Map Text BattleDefinition
    , encounters :: [Encounter]
    , items :: Map Text Item
    , messages :: Map Text Text
    , moves :: Map MoveId Move
    , rules :: Rules
    , species :: Map Text Species
    , stages :: StageDefinitions
    , statNames :: Map StatName Text
    , statuses :: Map Text StatusDefinition
    }
    deriving stock (Eq)
    deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec CombatConfig)

defaultCombat :: CombatConfig
defaultCombat =
    CombatConfig
        { battles = Map.empty
        , encounters = []
        , items = Map.empty
        , messages = Map.empty
        , moves = Map.empty
        , rules = defaultRules
        , species = Map.empty
        , stages = StageDefinitions Map.empty
        , statNames = Map.empty
        , statuses = Map.empty
        }

instance HasCodec CombatConfig where
    codec =
        Codec.named "CombatConfig" $
            Codec.bimapCodec validate id $
                Codec.object "CombatConfig" $
                    CombatConfig
                        <$> Codec.optionalFieldWithOmittedDefault "battles" (Map.empty) "Named, scripted or encounter battles." Codec..= (.battles)
                        <*> Codec.optionalFieldWithOmittedDefault "encounters" ([]) "Encounter tables." Codec..= (.encounters)
                        <*> Codec.optionalFieldWithOmittedDefault "items" (Map.empty) "Battle item definitions." Codec..= (.items)
                        <*> Codec.optionalFieldWithOmittedDefault "messages" Map.empty "Game-defined battle message templates with expression placeholders." Codec..= (.messages)
                        <*> Codec.optionalFieldWithOmittedDefault "moves" (Map.empty) "Move definitions." Codec..= (.moves)
                        <*> Codec.optionalFieldWithOmittedDefault "rules" (defaultRules) "Game-defined combat and progression rules." Codec..= (.rules)
                        <*> Codec.optionalFieldWithOmittedDefault "species" (Map.empty) "Species stats, types and learnsets." Codec..= (.species)
                        <*> Codec.optionalFieldWithOmittedDefault "stages" (StageDefinitions Map.empty) "Game-defined temporary stage limits and multipliers, keyed by stat name." Codec..= (.stages)
                        <*> Codec.optionalFieldWithOmittedDefault "statNames" Map.empty "Display labels keyed by stat name." Codec..= (.statNames)
                        <*> Codec.optionalFieldWithOmittedDefault "statuses" (Map.empty) "Persistent status definitions." Codec..= (.statuses)
      where
        validate value
            | any (Text.null . Text.strip) (Map.keys value.battles <> Map.keys value.items <> Map.keys value.species <> Map.keys value.statuses) = Left "Combat identifiers must not be blank."
            | any (\(MoveId identifier) -> Text.null (Text.strip identifier)) (Map.keys value.moves) = Left "Move identifiers must not be blank."
            | any (not . knownMove) (concatMap (concat . Map.elems . (.learnset)) (Map.elems value.species)) = Left "A species learnset references a missing move."
            | any (not . validEffect) (value.rules.fallbackMove.effects <> concatMap (.effects) (Map.elems value.moves)) = Left "A move effect references an unknown status or invalid stat."
            | any (not . validCritter) (concatMap (.opponents) (Map.elems value.battles)) = Left "An opposing critter references an unknown species, move or status."
            | any (not . validEncounterSpecies) entries = Left "An encounter references a missing species."
            | any (not . validEncounterBattle) entries = Left "An encounter references a missing battle."
            | any (not . validEncounterLevel) entries = Left "Encounter levels exceed the game level limit."
            | any (not . (`Map.member` value.statuses)) (Map.keys value.rules.statusCaptureBonuses) = Left "A capture bonus references a missing status."
            | otherwise = Right value
          where
            knownMove identifier = Map.member identifier value.moves
            entries = concatMap (.entries) value.encounters
            validEncounterBattle = \case
                Entry.EncounterEntry{} -> True
                Entry.LegacyEncounterEntry{Entry.battle} -> Map.member battle value.battles
            validEncounterLevel = \case
                Entry.EncounterEntry{Entry.level} -> level <= value.rules.maxLevel
                Entry.LegacyEncounterEntry{Entry.maxLevel, Entry.minLevel} -> all (maybe True (<= value.rules.maxLevel)) [maxLevel, minLevel]
            validEncounterSpecies = \case
                Entry.EncounterEntry{Entry.species} -> Map.member species value.species
                Entry.LegacyEncounterEntry{} -> True
            validCritter critter = Map.member critter.species value.species && all knownMove critter.battleProfile.knownMoves && maybe True (\affliction -> Map.member affliction.status value.statuses) critter.battleProfile.status
            validEffect = \case
                Chance _ effect -> validEffect effect
                ChangeHealth _ formula -> either (const False) (all (`elem` ["damage", "hp", "maxHP", "level", "targetHP", "targetMaxHP"])) (Expression.variables formula)
                ChangeStage{} -> True
                ClearStatus _ -> True
                InflictStatus _ identifier -> Map.member identifier value.statuses
