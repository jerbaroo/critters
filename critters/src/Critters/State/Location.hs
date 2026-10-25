module Critters.State.Location (Location (..)) where

import Autodocodec (Autodocodec (..), HasCodec)
import Autodocodec qualified as Codec
import Critters.Pretty (Pretty (..))
import Critters.State.MapId (MapId)
import Critters.State.Position (Position)
import Data.Aeson qualified as Aeson

data Location = Location
    { mapId :: MapId
    , position :: Position
    }
    deriving stock (Eq, Ord)
    deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec Location)

instance HasCodec Location where
    codec =
        Codec.object "Location" $
            Location
                <$> Codec.requiredField "mapId" "The map identifier." Codec..= (.mapId)
                <*> Codec.requiredField "position" "Coordinates within that map." Codec..= (.position)

instance Pretty Location where
    pretty (Location identifier coordinates) = pretty identifier <> " " <> pretty coordinates
