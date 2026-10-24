module Critters.State.World (tileAt, World (..)) where

import Autodocodec (Autodocodec (..), HasCodec)
import Autodocodec qualified as Codec
import Critters.State.Position (Position (..))
import Critters.State.Tile (Tile)
import Data.Aeson qualified as Aeson

-- | Missing cells and negative coordinates return 'Nothing'.
tileAt :: World -> Position -> Maybe Tile
tileAt (World rows) (Position column row)
  | row < 0 = Nothing
  | otherwise = do
      cells <- at (length rows - row - 1) rows
      at column cells
  where
    at :: Int -> [a] -> Maybe a
    at index values
      | index < 0 = Nothing
      | otherwise = case drop index values of
          value : _ -> Just value
          [] -> Nothing

-- | Rows of tiles, ordered from top to bottom and left to right. The last row is y = 0.
newtype World = World
  { tiles :: [[Tile]]
  }
  deriving stock (Eq)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec World)

instance HasCodec World where
  codec =
    Codec.object "World" $
      World
        <$> Codec.requiredField "tiles" "Rows from top to bottom (last row has y = 0); tiles within a row run left to right." Codec..= tiles
