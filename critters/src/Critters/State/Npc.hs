module Critters.State.Npc (Npc (..)) where

import Autodocodec (Autodocodec (..), HasCodec)
import Autodocodec qualified as Codec
import Critters.State.Location (Location (..))
import Critters.State.MapId (MapId (..))
import Data.Aeson qualified as Aeson
import Data.Text (Text)

data Npc = Npc
  { dialogue :: [Text],
    location :: Location
  }
  deriving stock (Eq)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec Npc)

instance HasCodec Npc where
  codec =
    Codec.object "Npc" $ Codec.parseAlternative current legacy
    where
      current =
        Npc
          <$> Codec.requiredField "dialogue" "Dialogue lines in order; Enter advances to the next line." Codec..= dialogue
          <*> Codec.requiredField "location" "The map and ground cell occupied by the NPC." Codec..= location
      legacy =
        Npc
          <$> Codec.requiredField "dialogue" "Dialogue lines in order." Codec..= dialogue
          <*> (Location (MapId "overworld") <$> Codec.requiredField "position" "Coordinates in the overworld." Codec..= (position . location))
