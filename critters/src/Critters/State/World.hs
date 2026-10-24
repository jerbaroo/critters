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
      cell <- at column cells
      cell
  where
    at :: Int -> [a] -> Maybe a
    at index values
      | index < 0 = Nothing
      | otherwise = case drop index values of
          value : _ -> Just value
          [] -> Nothing

-- | Rows from top to bottom, with missing cells represented by 'Nothing'. The last row is y = 0.
newtype World = World
  { tiles :: [[Maybe Tile]]
  }
  deriving stock (Eq)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec World)

instance HasCodec World where
  codec =
    Codec.object "World" $
      World
        <$> Codec.requiredField "tiles" "Rows from top to bottom (last row has y = 0); cells run left to right, with null for missing cells." Codec..= tiles
