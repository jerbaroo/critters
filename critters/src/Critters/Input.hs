module Critters.Input (Input (..), parse) where

import Critters.Pretty (Pretty (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Prelude (Char, Eq, Maybe (..), (<>))

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
parse :: Text -> Maybe Input
parse input = case Text.strip input of
  "" -> Just Enter
  "down" -> Just Down
  "enter" -> Just Enter
  "escape" -> Just Escape
  "left" -> Just Left
  "right" -> Just Right
  "up" -> Just Up
  value -> case Text.uncons value of
    Just (character, rest) | Text.null rest -> Just (Character character)
    _ -> Nothing

instance Pretty Input where
  pretty = \case
    Character character -> "Character '" <> Text.singleton character <> "'"
    Down -> "Down"
    Enter -> "Enter"
    Escape -> "Escape"
    Left -> "Left"
    Right -> "Right"
    Up -> "Up"
