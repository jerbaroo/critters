module Critters.State.Npc (Npc (..)) where

import Autodocodec (Autodocodec (..), HasCodec)
import Autodocodec qualified as Codec
import Critters.State.Interaction (Interaction)
import Critters.State.Location (Location)
import Data.Aeson qualified as Aeson
import Data.Text (Text)

data Npc = Npc
    { dialogue :: [Text]
    , interactions :: [Interaction]
    , location :: Location
    , name :: Text
    }
    deriving stock (Eq)
    deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec Npc)

instance HasCodec Npc where
    codec =
        Codec.object "Npc" $
            Npc
                <$> Codec.optionalFieldWithOmittedDefault "dialogue" [] "Legacy dialogue, used when interactions are absent." Codec..= (.dialogue)
                <*> Codec.optionalFieldWithOmittedDefault "interactions" [] "Conditional interactions in priority order." Codec..= (.interactions)
                <*> Codec.requiredField "location" "The map and ground cell occupied by the NPC." Codec..= (.location)
                <*> Codec.requiredField "name" "The display name referenced by text as {npcId.name}." Codec..= (.name)
