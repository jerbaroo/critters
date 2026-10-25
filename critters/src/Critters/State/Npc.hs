module Critters.State.Npc (Npc (..)) where

import Autodocodec (Autodocodec (..), HasCodec, JSONObjectCodec)
import Autodocodec qualified as Codec
import Critters.State.Interaction (Interaction)
import Critters.State.Location (Location (..))
import Critters.State.MapId (MapId (..))
import Data.Aeson qualified as Aeson
import Data.Text (Text)

data Npc = Npc
    { dialogue :: [Text]
    , interactions :: [Interaction]
    , location :: Location
    }
    deriving stock (Eq)
    deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec Npc)

instance HasCodec Npc where
    codec =
        Codec.object "Npc" $ Codec.parseAlternative current legacy
      where
        current :: JSONObjectCodec Npc
        current =
            Npc
                <$> Codec.optionalFieldWithOmittedDefault "dialogue" [] "Legacy dialogue, used when interactions are absent." Codec..= (.dialogue)
                <*> Codec.optionalFieldWithOmittedDefault "interactions" [] "Conditional interactions in priority order." Codec..= (.interactions)
                <*> Codec.requiredField "location" "The map and ground cell occupied by the NPC." Codec..= (.location)
        legacy :: JSONObjectCodec Npc
        legacy =
            Npc
                <$> Codec.optionalFieldWithOmittedDefault "dialogue" [] "Legacy dialogue lines in order." Codec..= (.dialogue)
                <*> Codec.optionalFieldWithOmittedDefault "interactions" [] "Conditional interactions in priority order." Codec..= (.interactions)
                <*> (Location (MapId "overworld") <$> Codec.requiredField "position" "Coordinates in the overworld." Codec..= (.location.position))
