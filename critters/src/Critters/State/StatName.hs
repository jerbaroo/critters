module Critters.State.StatName (StatName (..)) where

import Autodocodec (Autodocodec (..), HasCodec)
import Autodocodec qualified as Codec
import Critters.Pretty (Pretty (..))
import Data.Aeson qualified as Aeson
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as Text

newtype StatName = StatName Text
    deriving stock (Eq, Ord, Show)
    deriving newtype (Aeson.FromJSONKey, Aeson.ToJSONKey, IsString)
    deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec StatName)

instance HasCodec StatName where
    codec = Codec.bimapCodec validate (\(StatName name) -> name) Codec.codec
      where
        validate name
            | Text.null (Text.strip name) = Left "Stat names must not be blank."
            | otherwise = Right (StatName name)

instance Pretty StatName where
    pretty (StatName name) = name
