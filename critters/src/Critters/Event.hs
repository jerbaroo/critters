module Critters.Event (Event (..)) where

import Autodocodec (Autodocodec (..), HasCodec)
import Autodocodec qualified as Codec
import Critters.State.Position (Position)
import Data.Aeson qualified as Aeson

-- | An established fact. Validation happens before an event is produced.
data Event
  = PlayerMoved{position :: Position}
  deriving stock (Eq)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec Event)

instance HasCodec Event where
  codec =
    Codec.object "Event" $
      PlayerMoved
        <$> Codec.requiredField "position" "The player's position after moving." Codec..= position
        <* Codec.requiredFieldWith "type" (Codec.literalTextCodec "player-moved") "The event type." Codec..= const "player-moved"
