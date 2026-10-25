module Critters.State.Position (Position (..)) where

import Autodocodec (Autodocodec (..), HasCodec)
import Autodocodec qualified as Codec
import Critters.Pretty (Pretty (..))
import Data.Aeson qualified as Aeson
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy.Builder.Int (decimal)
import Witch (from)

-- | Zero-based coordinates from the bottom-left: x increases rightwards, y upwards.
data Position = Position
    { x :: Int
    , y :: Int
    }
    deriving stock (Eq, Ord)
    deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec Position)

instance HasCodec Position where
    codec =
        Codec.object "Position" $
            Position
                <$> Codec.requiredField "x" "Zero-based column, increasing to the right." Codec..= (.x)
                <*> Codec.requiredField "y" "Zero-based row from the bottom, increasing upwards." Codec..= (.y)

instance Pretty Position where
    pretty (Position column row) =
        from $ toLazyText $ "(" <> decimal column <> ", " <> decimal row <> ")"
