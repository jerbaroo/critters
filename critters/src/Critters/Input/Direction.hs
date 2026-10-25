module Critters.Input.Direction (Direction (..)) where

import Autodocodec (Autodocodec (..), HasCodec)
import Autodocodec qualified as Codec
import Critters.Pretty (Pretty (..))
import Data.Aeson qualified as Aeson
import Data.List.NonEmpty (NonEmpty (..))

data Direction
    = East
    | North
    | South
    | West
    deriving stock (Eq)
    deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec Direction)

instance HasCodec Direction where
    codec = Codec.stringConstCodec $ (East, "east") :| [(North, "north"), (South, "south"), (West, "west")]

instance Pretty Direction where
    pretty = \case
        East -> "East"
        North -> "North"
        South -> "South"
        West -> "West"
