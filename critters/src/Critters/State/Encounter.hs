module Critters.State.Encounter (Encounter (..)) where

import Autodocodec qualified as Codec
import Autodocodec (Autodocodec (..), HasCodec)
import Critters.Codec qualified as NumberCodec
import Critters.State.EncounterEntry (EncounterEntry)
import Critters.State.MapId (MapId)
import Critters.State.Tile (Tile (..))
import Data.Aeson qualified as Aeson

data Encounter = Encounter
  { encounterChance :: Double,
    encounterEntries :: [EncounterEntry],
    encounterMapId :: MapId,
    encounterMinimumSteps :: Int,
    encounterTiles :: [Tile]
  }
  deriving stock (Eq)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec Encounter)

instance HasCodec Encounter where
  codec = Codec.bimapCodec validate id $ Codec.object "Encounter" $ Encounter
    <$> Codec.optionalFieldWithOmittedDefaultWith "chance" NumberCodec.probabilityCodec (0) "Probability per eligible movement step." Codec..= encounterChance
    <*> Codec.optionalFieldWithOmittedDefault "entries" ([]) "Weighted encounter choices." Codec..= encounterEntries
    <*> Codec.requiredField "mapId" "Map containing the encounter area." Codec..= encounterMapId
    <*> Codec.optionalFieldWithOmittedDefault "minimumSteps" (0) "Minimum eligible movement steps between encounters." Codec..= encounterMinimumSteps
    <*> Codec.optionalFieldWithOmittedDefault "tiles" ([TallGrass]) "Terrain eligible for this table." Codec..= encounterTiles
    where
      validate value
        | encounterMinimumSteps value < 0 || (encounterChance value > 0 && null (encounterEntries value)) = Left "Step counts must be nonnegative and enabled tables need entries."
        | otherwise = Right value
