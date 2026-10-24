module Critters.Input.Direction (Direction (..)) where

import Critters.Pretty (Pretty (..))

data Direction
  = East
  | North
  | South
  | West
  deriving stock (Eq)

instance Pretty Direction where
  pretty = \case
    East -> "East"
    North -> "North"
    South -> "South"
    West -> "West"
