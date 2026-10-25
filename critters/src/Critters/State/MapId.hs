module Critters.State.MapId (MapId (..)) where

import Autodocodec (Autodocodec (..), HasCodec)
import Autodocodec qualified as Codec
import Critters.Pretty (Pretty (..))
import Data.Aeson qualified as Aeson
import Data.Text (Text)

newtype MapId = MapId Text
    deriving stock (Eq, Ord)
    deriving newtype (Aeson.FromJSONKey, Aeson.ToJSONKey)
    deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec MapId)

instance HasCodec MapId where
    codec = Codec.dimapCodec MapId (\(MapId name) -> name) Codec.codec

instance Pretty MapId where
    pretty (MapId name) = name
