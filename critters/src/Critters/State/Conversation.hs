module Critters.State.Conversation (Conversation (..)) where

import Autodocodec (Autodocodec (..), HasCodec)
import Autodocodec qualified as Codec
import Critters.State.NpcId (NpcId)
import Data.Aeson qualified as Aeson

data Conversation = Conversation
    { lineIndex :: Int
    , npcId :: NpcId
    }
    deriving stock (Eq)
    deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec Conversation)

instance HasCodec Conversation where
    codec =
        Codec.object "Conversation" $
            Conversation
                <$> Codec.requiredField "lineIndex" "Zero-based index of the current dialogue line." Codec..= (.lineIndex)
                <*> Codec.requiredField "npcId" "The NPC being spoken to." Codec..= (.npcId)
