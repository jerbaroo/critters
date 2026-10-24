module Critters.State.Warp (Warp (..)) where

import Autodocodec (Autodocodec (..), HasCodec)
import Autodocodec qualified as Codec
import Critters.Input.Direction (Direction)
import Critters.State.Location (Location)
import Critters.State.Position (Position)
import Data.Aeson qualified as Aeson

-- | Entering the source cell in an allowed direction transports the player,
-- regardless of its terrain. 'Nothing' permits entry from any direction.
data Warp = Warp
  { destination :: Location,
    entranceDirection :: Maybe Direction,
    source :: Position
  }
  deriving stock (Eq)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec Warp)

instance HasCodec Warp where
  codec =
    Codec.object "Warp" $
      Warp
        <$> Codec.requiredField "destination" "Where the player arrives." Codec..= destination
        <*> Codec.optionalFieldOrNull "entranceDirection" "Required movement direction when entering the source cell; omitted or null allows any direction." Codec..= entranceDirection
        <*> Codec.requiredField "source" "The cell that triggers this warp when entered." Codec..= source
