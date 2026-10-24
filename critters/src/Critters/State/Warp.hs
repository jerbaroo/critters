module Critters.State.Warp (Warp (..)) where

import Autodocodec (Autodocodec (..), HasCodec)
import Autodocodec qualified as Codec
import Critters.State.Location (Location)
import Critters.State.Position (Position)
import Data.Aeson qualified as Aeson

-- | Entering the source cell transports the player, regardless of its terrain.
data Warp = Warp
  { destination :: Location,
    source :: Position
  }
  deriving stock (Eq)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec Warp)

instance HasCodec Warp where
  codec =
    Codec.object "Warp" $
      Warp
        <$> Codec.requiredField "destination" "Where the player arrives." Codec..= destination
        <*> Codec.requiredField "source" "The cell that triggers this warp when entered." Codec..= source
