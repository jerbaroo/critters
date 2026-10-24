module State.Tile (Tile (..)) where

import Autodocodec (Autodocodec (..), HasCodec)
import Autodocodec qualified as Codec
import Data.Aeson qualified as Aeson
import Data.List.NonEmpty (NonEmpty (..))

data Tile
  = Ground
  | TallGrass
  | Wall
  | Water
  deriving stock (Eq)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec Tile)

instance HasCodec Tile where
  codec =
    Codec.stringConstCodec $
      (Ground, "ground")
        :| [ (TallGrass, "tall-grass"),
             (Wall, "wall"),
             (Water, "water")
           ]
