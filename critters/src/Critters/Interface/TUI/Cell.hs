module Critters.Interface.TUI.Cell (Cell (..), columns) where

import Data.Char.WCWidth (wcwidth)
import Data.Text (Text)
import Data.Text qualified as Text

-- | A glyph and any combining marks, including its appearance.
data Cell = Cell
    { glyph :: Text
    , whiteBackground :: Bool
    }
    deriving stock (Eq)

columns :: Cell -> Int
columns = Text.foldl' (\total value -> total + max 0 (wcwidth value)) 0 . (.glyph)
