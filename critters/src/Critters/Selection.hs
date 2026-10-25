module Critters.Selection (moveSelection) where

import Critters.Input.Direction (Direction (..))

-- | Move within a row-major menu, staying put at edges and missing entries.
moveSelection :: Int -> Int -> Direction -> Int -> Int
moveSelection columns count direction index
    | columns <= 0 || index < 0 || index >= count = index
    | nextRow < 0 || nextColumn < 0 || nextColumn >= columns || next >= count = index
    | otherwise = next
  where
    (row, column) = index `divMod` columns
    next = nextRow * columns + nextColumn
    (nextRow, nextColumn) = case direction of
        East -> (row, column + 1)
        North -> (row - 1, column)
        South -> (row + 1, column)
        West -> (row, column - 1)
