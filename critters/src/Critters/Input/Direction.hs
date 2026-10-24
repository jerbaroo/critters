module Critters.Input.Direction (Direction (..)) where

data Direction
  = East
  | North
  | South
  | West
  deriving stock (Eq)
