module Critters.State.Npc (Npc (..)) where

import Autodocodec (Autodocodec (..), HasCodec)
import Autodocodec qualified as Codec
import Critters.State.Position (Position)
import Data.Aeson qualified as Aeson
import Data.Text (Text)

data Npc = Npc
  { dialogue :: [Text],
    position :: Position
  }
  deriving stock (Eq)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec Npc)

instance HasCodec Npc where
  codec =
    Codec.object "Npc" $
      Npc
        <$> Codec.requiredField "dialogue" "Dialogue lines in order; Enter advances to the next line." Codec..= dialogue
        <*> Codec.requiredField "position" "The ground cell occupied by the NPC." Codec..= position
