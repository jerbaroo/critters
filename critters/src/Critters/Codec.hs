module Critters.Codec (finiteDoubleCodec, probabilityCodec) where

import Autodocodec (JSONCodec)
import Autodocodec qualified as Codec

finiteDoubleCodec :: JSONCodec Double
finiteDoubleCodec = Codec.bimapCodec validate id (Codec.codecViaAeson "FiniteDouble")
  where
    validate value
      | isNaN value || isInfinite value = Left "Numbers must be finite."
      | otherwise = Right value

probabilityCodec :: JSONCodec Double
probabilityCodec = Codec.bimapCodec validate id finiteDoubleCodec
  where
    validate value
      | value < 0 || value > 1 = Left "Probability must be between zero and one."
      | otherwise = Right value
