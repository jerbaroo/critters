module Critters.State.Encounter (Encounter (..)) where

import Autodocodec (Autodocodec (..), HasCodec)
import Autodocodec qualified as Codec
import Critters.Codec qualified as NumberCodec
import Critters.State.EncounterEntry (EncounterEntry)
import Critters.State.MapId (MapId)
import Critters.State.Tile (Tile (..))
import Data.Aeson qualified as Aeson

data Encounter = Encounter
    { chance :: Double
    , entries :: [EncounterEntry]
    , mapId :: MapId
    , minimumSteps :: Int
    , tiles :: [Tile]
    }
    deriving stock (Eq)
    deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec Encounter)

instance HasCodec Encounter where
    codec =
        Codec.bimapCodec validate id $
            Codec.object "Encounter" $
                Encounter
                    <$> Codec.optionalFieldWithOmittedDefaultWith "chance" NumberCodec.probabilityCodec (0) "Probability per eligible movement step." Codec..= (.chance)
                    <*> Codec.optionalFieldWithOmittedDefault "entries" ([]) "Weighted encounter choices." Codec..= (.entries)
                    <*> Codec.requiredField "mapId" "Map containing the encounter area." Codec..= (.mapId)
                    <*> Codec.optionalFieldWithOmittedDefault "minimumSteps" (0) "Minimum eligible movement steps between encounters." Codec..= (.minimumSteps)
                    <*> Codec.optionalFieldWithOmittedDefault "tiles" ([TallGrass]) "Terrain eligible for this table." Codec..= (.tiles)
      where
        validate value
            | value.minimumSteps < 0 || (value.chance > 0 && null value.entries) = Left "Step counts must be nonnegative and enabled tables need entries."
            | otherwise = Right value
