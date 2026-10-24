module Critters.State.MoveId (MoveId (..)) where

import Autodocodec (Autodocodec (..), HasCodec)
import Autodocodec qualified as Codec
import Critters.Pretty (Pretty (..))
import Data.Aeson qualified as Aeson
import Data.Text (Text)

newtype MoveId = MoveId Text
  deriving stock (Eq, Ord)
  deriving newtype (Aeson.FromJSONKey, Aeson.ToJSONKey)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec MoveId)

instance HasCodec MoveId where
  codec = Codec.dimapCodec MoveId (\(MoveId value) -> value) Codec.codec

instance Pretty MoveId where
  pretty (MoveId value) = value
