module Critters.Input (Input (..), parse) where

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

-- | Decode line-based controls without interpreting their game meaning.
parse :: String -> Maybe Input
parse = \case
  "" -> Just Enter
  "down" -> Just Down
  "enter" -> Just Enter
  "escape" -> Just Escape
  "left" -> Just Left
  "right" -> Just Right
  "up" -> Just Up
  [character] -> Just (Character character)
  _ -> Nothing
