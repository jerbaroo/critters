module Critters.State.Species (BaseStats (..), defaultSpecies, Species (..)) where

import Autodocodec (Autodocodec (..), HasCodec)
import Autodocodec qualified as Codec
import Critters.Codec qualified as NumberCodec
import Critters.State.MoveId (MoveId)
import Data.Aeson qualified as Aeson
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)

newtype BaseStats = BaseStats {values :: Map Text Int}
    deriving stock (Eq)
    deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec BaseStats)

instance HasCodec BaseStats where
    codec = Codec.dimapCodec BaseStats (.values) (Codec.mapCodec Codec.codec)

defaultSpecies :: Species
defaultSpecies =
    Species
        { captureRate = 0.5
        , experienceYield = 10
        , learnset = Map.empty
        , name = ""
        , stats = BaseStats (Map.fromList [("attack", 10), ("defense", 10), ("health", 20), ("speed", 10)])
        , types = []
        }

data Species = Species
    { captureRate :: Double
    , experienceYield :: Int
    , learnset :: Map Int [MoveId]
    , name :: Text
    , stats :: BaseStats
    , types :: [Text]
    }
    deriving stock (Eq)
    deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec Species)

instance HasCodec Species where
    codec =
        Codec.bimapCodec validate id $
            Codec.object "Species" $
                Species
                    <$> Codec.optionalFieldWithOmittedDefaultWith "captureRate" NumberCodec.probabilityCodec (0.5) "Base capture probability." Codec..= (.captureRate)
                    <*> Codec.optionalFieldWithOmittedDefault "experienceYield" (10) "Experience awarded by this species." Codec..= (.experienceYield)
                    <*> Codec.optionalFieldWithOmittedDefault "learnset" (Map.empty) "Moves learned at each level." Codec..= (.learnset)
                    <*> Codec.optionalFieldWithOmittedDefault "name" ("") "Optional display name." Codec..= (.name)
                    <*> Codec.optionalFieldWithOmittedDefault "baseStats" (BaseStats (Map.fromList [("attack", 10), ("defense", 10), ("health", 20), ("speed", 10)])) "Positive base stats; move definitions choose stat identifiers." Codec..= (.stats)
                    <*> Codec.optionalFieldWithOmittedDefault "types" ([]) "Game-defined types." Codec..= (.types)
      where
        validate value
            | value.experienceYield < 0 = Left "Experience yield must not be negative."
            | any (< 1) (Map.keys value.learnset) || any (< 1) (Map.elems value.stats.values) = Left "Learnset levels and base stats must be positive."
            | otherwise = Right value
