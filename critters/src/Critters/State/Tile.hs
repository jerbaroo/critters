module Critters.State.Tile (Tile (..)) where

import Autodocodec (Autodocodec (..), HasCodec)
import Autodocodec qualified as Codec
import Critters.Input.Direction (Direction (..))
import Data.Aeson qualified as Aeson
import Data.List.NonEmpty (NonEmpty (..))

-- | Terrain and occupied ground cells, independent of their visual projection.
data Tile
    = Arena
    | Clinic
    | Door
    | Fence
    | Flowers
    | Grass
    | Ground
    | House
    | Laboratory
    | Ledge Direction
    | Path
    | Rock
    | Sand
    | Shop
    | Sign
    | Stairs
    | TallGrass
    | Tree
    | Wall
    | Water
    deriving stock (Eq)
    deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec Tile)

instance HasCodec Tile where
    codec =
        Codec.named "Tile" $
            Codec.stringConstCodec $
                (Arena, "arena")
                    :| [ (Clinic, "clinic")
                       , (Door, "door")
                       , (Fence, "fence")
                       , (Flowers, "flowers")
                       , (Grass, "grass")
                       , (Ground, "ground")
                       , (House, "house")
                       , (Laboratory, "laboratory")
                       , (Ledge East, "ledge-east")
                       , (Ledge North, "ledge-north")
                       , (Ledge South, "ledge-south")
                       , (Ledge West, "ledge-west")
                       , (Path, "path")
                       , (Rock, "rock")
                       , (Sand, "sand")
                       , (Shop, "shop")
                       , (Sign, "sign")
                       , (Stairs, "stairs")
                       , (TallGrass, "tall-grass")
                       , (Tree, "tree")
                       , (Wall, "wall")
                       , (Water, "water")
                       ]
