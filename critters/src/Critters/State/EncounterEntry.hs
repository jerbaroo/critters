module Critters.State.EncounterEntry (EncounterEntry (..)) where

import Autodocodec (Autodocodec (..), HasCodec)
import Autodocodec qualified as Codec
import Data.Aeson qualified as Aeson
import Data.Text (Text)

data EncounterEntry = EncounterEntry
  { entryBattle :: Text,
    entryMaxLevel :: Maybe Int,
    entryMinLevel :: Maybe Int,
    entryWeight :: Int
  }
  deriving stock (Eq)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec EncounterEntry)

instance HasCodec EncounterEntry where
  codec =
    Codec.bimapCodec validate id
      $ Codec.object "EncounterEntry"
      $ EncounterEntry
        <$> Codec.requiredField "battle" "Configured battle identifier." Codec..= entryBattle
        <*> Codec.optionalFieldOrNull "maxLevel" "Optional maximum generated opponent level." Codec..= entryMaxLevel
        <*> Codec.optionalFieldOrNull "minLevel" "Optional minimum generated opponent level." Codec..= entryMinLevel
        <*> Codec.optionalFieldWithOmittedDefault "weight" (1) "Positive selection weight." Codec..= entryWeight
    where
      validate value
        | entryWeight value < 1 || maybe False (< 1) (entryMinLevel value) || maybe False (< 1) (entryMaxLevel value) = Left "Weights and level bounds must be positive."
        | maybe False id ((>) <$> entryMinLevel value <*> entryMaxLevel value) = Left "Minimum level must not exceed maximum level."
        | otherwise = Right value
