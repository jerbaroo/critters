module Critters.State.World (tileAt, World (..)) where

import Autodocodec (Autodocodec (..), HasCodec)
import Autodocodec qualified as Codec
import Critters.State.Position (Position (..))
import Critters.State.Tile (Tile)
import Critters.State.Warp (Warp)
import Data.Aeson qualified as Aeson

-- | Missing cells and negative coordinates return 'Nothing'.
tileAt :: World -> Position -> Maybe Tile
tileAt (World rows _) (Position column row)
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

-- | A tile map. The last row is y = 0; 'Nothing' represents a missing cell.
data World = World
  { tiles :: [[Maybe Tile]],
    warps :: [Warp]
  }
  deriving stock (Eq)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec World)

instance HasCodec World where
  codec =
    Codec.object "World" $
      World
        <$> Codec.requiredField "tiles" "Rows from top to bottom (last row has y = 0); cells run left to right, with null for missing cells." Codec..= tiles
        <*> Codec.optionalFieldWithOmittedDefault "warps" [] "Position-based transitions to other maps." Codec..= warps
