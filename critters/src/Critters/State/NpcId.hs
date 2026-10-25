module Critters.State.NpcId (NpcId (..)) where

import Autodocodec (Autodocodec (..), HasCodec)
import Autodocodec qualified as Codec
import Critters.Pretty (Pretty (..))
import Data.Aeson qualified as Aeson
import Data.Text (Text)

newtype NpcId = NpcId Text
    deriving stock (Eq, Ord)
    deriving newtype (Aeson.FromJSONKey, Aeson.ToJSONKey)
    deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec NpcId)

instance HasCodec NpcId where
    codec = Codec.dimapCodec NpcId (\(NpcId name) -> name) Codec.codec

instance Pretty NpcId where
    pretty (NpcId name) = name
