module Critters.State.Tile (Tile (..)) where

import Autodocodec (Autodocodec (..), HasCodec)
import Autodocodec qualified as Codec
import Data.Aeson qualified as Aeson
import Data.List.NonEmpty (NonEmpty (..))

-- | Terrain and occupied ground cells, independent of their visual projection.
data Tile
  = Door
  | Flowers
  | Grass
  | Ground
  | House
  | Laboratory
  | Path
  | Sand
  | Sign
  | TallGrass
  | Tree
  | Wall
  | Water
  deriving stock (Eq)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec Tile)

instance HasCodec Tile where
  codec =
    Codec.stringConstCodec $
      (Door, "door")
        :| [ (Flowers, "flowers"),
             (Grass, "grass"),
             (Ground, "ground"),
             (House, "house"),
             (Laboratory, "laboratory"),
             (Path, "path"),
             (Sand, "sand"),
             (Sign, "sign"),
             (TallGrass, "tall-grass"),
             (Tree, "tree"),
             (Wall, "wall"),
             (Water, "water")
           ]
