module Critters.Number (integer, number) where

import Witch (tryFrom)

-- | Round down and saturate at the machine integer bounds.
integer :: Double -> Int
integer value
    | isNaN value = 0
    | isInfinite value = if value > 0 then maxBound else minBound
    | otherwise = either (const (if value > 0 then maxBound else minBound)) id (tryFrom @Integer @Int (floor value))

-- | Checked conversion, rounding integers larger than the exact floating range.
number :: Int -> Double
number value = case tryFrom @Int @Double value of
    Right result -> result
    Left _ -> 2 * number (value `quot` 2) + number (value `rem` 2)
