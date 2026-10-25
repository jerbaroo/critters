module Critters.State.EncounterEntry (EncounterEntry (..)) where

import Autodocodec (Autodocodec (..), HasCodec)
import Autodocodec qualified as Codec
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Data.Text qualified as Text

data EncounterEntry
    = EncounterEntry
        { allowCapture :: Bool
        , allowEscape :: Bool
        , level :: Int
        , species :: Text
        , weight :: Int
        }
    | LegacyEncounterEntry
        { battle :: Text
        , maxLevel :: Maybe Int
        , minLevel :: Maybe Int
        , weight :: Int
        }
    deriving stock (Eq)
    deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec EncounterEntry)

data EncounterEntryFields = EncounterEntryFields
    { allowCapture :: Bool
    , allowEscape :: Bool
    , battle :: Maybe Text
    , level :: Maybe Int
    , maxLevel :: Maybe Int
    , minLevel :: Maybe Int
    , species :: Maybe Text
    , weight :: Int
    }
    deriving stock (Eq)
    deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec EncounterEntryFields)

instance HasCodec EncounterEntry where
    codec = Codec.bimapCodec decode encode Codec.codec
      where
        decode fields
            | fields.weight < 1 = Left "Encounter weights must be positive."
            | Just species <- fields.species
            , Just level <- fields.level
            , fields.battle == Nothing
            , fields.maxLevel == Nothing
            , fields.minLevel == Nothing =
                if level < 1 || Text.null (Text.strip species)
                    then Left "Encounter levels must be positive and species identifiers must not be blank."
                    else Right $ EncounterEntry fields.allowCapture fields.allowEscape level species fields.weight
            | Just battle <- fields.battle
            , fields.level == Nothing
            , fields.species == Nothing =
                if Text.null (Text.strip battle) || maybe False (< 1) fields.minLevel || maybe False (< 1) fields.maxLevel
                    then Left "Legacy encounter battle identifiers and level bounds must be valid."
                    else case (fields.minLevel, fields.maxLevel) of
                        (Just minimumLevel, Just maximumLevel) | minimumLevel > maximumLevel -> Left "Minimum level must not exceed maximum level."
                        _ -> Right $ LegacyEncounterEntry battle fields.maxLevel fields.minLevel fields.weight
            | otherwise = Left "An encounter entry must specify either a species and level or a legacy battle reference."
        encode = \case
            EncounterEntry{allowCapture, allowEscape, level, species, weight} ->
                EncounterEntryFields allowCapture allowEscape Nothing (Just level) Nothing Nothing (Just species) weight
            LegacyEncounterEntry{battle, maxLevel, minLevel, weight} ->
                EncounterEntryFields True True (Just battle) Nothing maxLevel minLevel Nothing weight

instance HasCodec EncounterEntryFields where
    codec =
        Codec.object "EncounterEntry" $
            EncounterEntryFields
                <$> Codec.optionalFieldWithOmittedDefault "allowCapture" True "Permit capture attempts in this encounter." Codec..= (.allowCapture)
                <*> Codec.optionalFieldWithOmittedDefault "allowEscape" True "Permit escape attempts from this encounter." Codec..= (.allowEscape)
                <*> Codec.optionalFieldOrNull "battle" "Legacy configured battle identifier." Codec..= (.battle)
                <*> Codec.optionalFieldOrNull "level" "Generated opponent level." Codec..= (.level)
                <*> Codec.optionalFieldOrNull "maxLevel" "Legacy maximum generated opponent level." Codec..= (.maxLevel)
                <*> Codec.optionalFieldOrNull "minLevel" "Legacy minimum generated opponent level." Codec..= (.minLevel)
                <*> Codec.optionalFieldOrNull "species" "Species identifier for the generated opponent." Codec..= (.species)
                <*> Codec.optionalFieldWithOmittedDefault "weight" (1) "Positive selection weight." Codec..= (.weight)
