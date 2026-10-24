module Critters.State.EncounterEntry (EncounterEntry (..)) where

import Autodocodec (Autodocodec (..), HasCodec)
import Autodocodec qualified as Codec
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Data.Text qualified as Text

data EncounterEntry
  = EncounterEntry
      { entryAllowCapture :: Bool,
        entryAllowEscape :: Bool,
        entryLevel :: Int,
        entrySpecies :: Text,
        entryWeight :: Int
      }
  | LegacyEncounterEntry
      { entryBattle :: Text,
        entryMaxLevel :: Maybe Int,
        entryMinLevel :: Maybe Int,
        entryWeight :: Int
      }
  deriving stock (Eq)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec EncounterEntry)

data EncounterEntryFields = EncounterEntryFields
  { fieldsAllowCapture :: Bool,
    fieldsAllowEscape :: Bool,
    fieldsBattle :: Maybe Text,
    fieldsLevel :: Maybe Int,
    fieldsMaxLevel :: Maybe Int,
    fieldsMinLevel :: Maybe Int,
    fieldsSpecies :: Maybe Text,
    fieldsWeight :: Int
  }
  deriving stock (Eq)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec EncounterEntryFields)

instance HasCodec EncounterEntry where
  codec = Codec.bimapCodec decode encode Codec.codec
    where
      decode fields
        | fieldsWeight fields < 1 = Left "Encounter weights must be positive."
        | Just species <- fieldsSpecies fields,
          Just level <- fieldsLevel fields,
          fieldsBattle fields == Nothing,
          fieldsMaxLevel fields == Nothing,
          fieldsMinLevel fields == Nothing =
            if level < 1 || Text.null (Text.strip species)
              then Left "Encounter levels must be positive and species identifiers must not be blank."
              else Right $ EncounterEntry (fieldsAllowCapture fields) (fieldsAllowEscape fields) level species (fieldsWeight fields)
        | Just battle <- fieldsBattle fields,
          fieldsLevel fields == Nothing,
          fieldsSpecies fields == Nothing =
            if Text.null (Text.strip battle) || maybe False (< 1) (fieldsMinLevel fields) || maybe False (< 1) (fieldsMaxLevel fields)
              then Left "Legacy encounter battle identifiers and level bounds must be valid."
              else case (fieldsMinLevel fields, fieldsMaxLevel fields) of
                (Just minimumLevel, Just maximumLevel) | minimumLevel > maximumLevel -> Left "Minimum level must not exceed maximum level."
                _ -> Right $ LegacyEncounterEntry battle (fieldsMaxLevel fields) (fieldsMinLevel fields) (fieldsWeight fields)
        | otherwise = Left "An encounter entry must specify either a species and level or a legacy battle reference."
      encode = \case
        EncounterEntry{entryAllowCapture, entryAllowEscape, entryLevel, entrySpecies, entryWeight} ->
          EncounterEntryFields entryAllowCapture entryAllowEscape Nothing (Just entryLevel) Nothing Nothing (Just entrySpecies) entryWeight
        LegacyEncounterEntry{entryBattle, entryMaxLevel, entryMinLevel, entryWeight} ->
          EncounterEntryFields True True (Just entryBattle) Nothing entryMaxLevel entryMinLevel Nothing entryWeight

instance HasCodec EncounterEntryFields where
  codec =
    Codec.object "EncounterEntry" $
      EncounterEntryFields
        <$> Codec.optionalFieldWithOmittedDefault "allowCapture" True "Permit capture attempts in this encounter." Codec..= fieldsAllowCapture
        <*> Codec.optionalFieldWithOmittedDefault "allowEscape" True "Permit escape attempts from this encounter." Codec..= fieldsAllowEscape
        <*> Codec.optionalFieldOrNull "battle" "Legacy configured battle identifier." Codec..= fieldsBattle
        <*> Codec.optionalFieldOrNull "level" "Generated opponent level." Codec..= fieldsLevel
        <*> Codec.optionalFieldOrNull "maxLevel" "Legacy maximum generated opponent level." Codec..= fieldsMaxLevel
        <*> Codec.optionalFieldOrNull "minLevel" "Legacy minimum generated opponent level." Codec..= fieldsMinLevel
        <*> Codec.optionalFieldOrNull "species" "Species identifier for the generated opponent." Codec..= fieldsSpecies
        <*> Codec.optionalFieldWithOmittedDefault "weight" (1) "Positive selection weight." Codec..= fieldsWeight
