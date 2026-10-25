module Critters.State.ObjectId (ObjectId (..)) where

import Autodocodec (Autodocodec (..), HasCodec)
import Autodocodec qualified as Codec
import Critters.Pretty (Pretty (..))
import Data.Aeson qualified as Aeson
import Data.Text (Text)

newtype ObjectId = ObjectId Text
    deriving stock (Eq, Ord)
    deriving newtype (Aeson.FromJSONKey, Aeson.ToJSONKey)
    deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec ObjectId)

instance HasCodec ObjectId where
    codec = Codec.dimapCodec ObjectId (\(ObjectId identifier) -> identifier) Codec.codec

instance Pretty ObjectId where
    pretty (ObjectId identifier) = identifier
