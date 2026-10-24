module Critters.State.Species (BaseStats (..), defaultSpecies, Species (..)) where

import Autodocodec (Autodocodec (..), HasCodec)
import Autodocodec qualified as Codec
import Critters.Codec qualified as NumberCodec
import Critters.State.MoveId (MoveId)
import Data.Aeson qualified as Aeson
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)

newtype BaseStats = BaseStats{unBaseStats :: Map Text Int}
  deriving stock (Eq)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec BaseStats)

instance HasCodec BaseStats where
  codec = Codec.dimapCodec BaseStats unBaseStats (Codec.mapCodec Codec.codec)

defaultSpecies :: Species
defaultSpecies =
  Species
    { speciesCaptureRate = 0.5,
      speciesExperienceYield = 10,
      speciesLearnset = Map.empty,
      speciesName = "",
      speciesStats = BaseStats (Map.fromList [("attack", 10), ("defense", 10), ("health", 20), ("speed", 10)]),
      speciesTypes = []
    }

data Species = Species
  { speciesCaptureRate :: Double,
    speciesExperienceYield :: Int,
    speciesLearnset :: Map Int [MoveId],
    speciesName :: Text,
    speciesStats :: BaseStats,
    speciesTypes :: [Text]
  }
  deriving stock (Eq)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec Species)

instance HasCodec Species where
  codec =
    Codec.bimapCodec validate id
      $ Codec.object "Species"
      $ Species
        <$> Codec.optionalFieldWithOmittedDefaultWith "captureRate" NumberCodec.probabilityCodec (0.5) "Base capture probability." Codec..= speciesCaptureRate
        <*> Codec.optionalFieldWithOmittedDefault "experienceYield" (10) "Experience awarded by this species." Codec..= speciesExperienceYield
        <*> Codec.optionalFieldWithOmittedDefault "learnset" (Map.empty) "Moves learned at each level." Codec..= speciesLearnset
        <*> Codec.optionalFieldWithOmittedDefault "name" ("") "Optional display name." Codec..= speciesName
        <*> Codec.optionalFieldWithOmittedDefault "baseStats" (BaseStats (Map.fromList [("attack", 10), ("defense", 10), ("health", 20), ("speed", 10)])) "Positive base stats; move definitions choose stat identifiers." Codec..= speciesStats
        <*> Codec.optionalFieldWithOmittedDefault "types" ([]) "Game-defined types." Codec..= speciesTypes
    where
      validate value
        | speciesExperienceYield value < 0 = Left "Experience yield must not be negative."
        | any (< 1) (Map.keys (speciesLearnset value)) || any (< 1) (Map.elems (unBaseStats (speciesStats value))) = Left "Learnset levels and base stats must be positive."
        | otherwise = Right value
