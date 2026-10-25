module Critters.State.FlagId (FlagId (..)) where

import Autodocodec (Autodocodec (..), HasCodec)
import Autodocodec qualified as Codec
import Critters.Pretty (Pretty (..))
import Data.Aeson qualified as Aeson
import Data.Text (Text)

newtype FlagId = FlagId Text
    deriving stock (Eq, Ord)
    deriving newtype (Aeson.FromJSONKey, Aeson.ToJSONKey)
    deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec FlagId)

instance HasCodec FlagId where
    codec = Codec.dimapCodec FlagId (\(FlagId identifier) -> identifier) Codec.codec

instance Pretty FlagId where
    pretty (FlagId identifier) = identifier
