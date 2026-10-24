module Critters.State.Object (Object (..)) where

import Autodocodec (Autodocodec (..), HasCodec)
import Autodocodec qualified as Codec
import Critters.State.Interaction (Interaction)
import Critters.State.Location (Location)
import Data.Aeson qualified as Aeson
import Data.Text (Text)

-- | A stationary interactable object, drawn over its terrain.
data Object = Object
  { objectGlyph :: Text,
    objectInteractions :: [Interaction],
    objectLocation :: Location
  }
  deriving stock (Eq)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec Object)

instance HasCodec Object where
  codec =
    Codec.object "Object" $
      Object
        <$> Codec.requiredField "glyph" "The object's display marker." Codec..= objectGlyph
        <*> Codec.optionalFieldWithOmittedDefault "interactions" [] "Conditional interactions in priority order." Codec..= objectInteractions
        <*> Codec.requiredField "location" "The map and cell occupied by the object." Codec..= objectLocation
