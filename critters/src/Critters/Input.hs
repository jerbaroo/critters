module Critters.Input (Input (..)) where

import Prelude hiding (Left, Right)

data Input
  = Character Char
  | Down
  | Enter
  | Escape
  | Left
  | Right
  | Up
  deriving stock (Eq)
