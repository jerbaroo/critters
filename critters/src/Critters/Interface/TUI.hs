module Critters.Interface.TUI (height, input, output, render, width, writeLog) where

import Critters.Input (Input)
import Critters.Interface.TUI.Keyboard qualified as Keyboard
import Critters.State.GameState (GameState (..))
import Critters.State.Position (Position (..))
import Critters.State.Tile (Tile (..))
import Critters.State.World (World (..), tileAt)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import System.IO (hFlush, stdout)

-- | Editable viewport dimensions, in tiles. Each tile uses two terminal columns.
height :: Int
height = 32

width :: Int
width = 64

input :: IO Input
input = do
  TextIO.putStr "> "
  hFlush stdout
  Keyboard.input >>= \case
    Just userInput -> pure userInput
    Nothing -> writeLog () "Unrecognized input." >> input

output :: GameState -> IO ()
output state = do
  TextIO.putStr $
    "\ESC[2J\ESC[H"
      <> Text.replace "\n" "\r\n" (render state)
      <> "Move: arrow keys | q: quit\r\n"
  hFlush stdout

-- | A fixed viewport with north at the top, padded outside the world.
render :: GameState -> Text
render state = Text.unlines rows
  where
    cell position
      | position == playerPosition state = "@ "
      | otherwise = case tileAt (world state) position of
          Just Ground -> ". "
          Just TallGrass -> "^ "
          Just Wall -> "##"
          Just Water -> "~~"
          Nothing -> "  "
    origin size extent coordinate = max 0 $ min (max 0 (extent - size)) (max 0 coordinate - size `div` 2)
    originX = origin width (maximum (0 : map length worldRows)) (x (playerPosition state))
    originY = origin height (length worldRows) (y (playerPosition state))
    rows =
      [ Text.concat [cell (Position (originX + column) (originY + row)) | column <- [0 .. width - 1]]
      | row <- reverse [0 .. height - 1]
      ]
    worldRows = tiles (world state)

writeLog :: () -> Text -> IO ()
writeLog _ message = do
  TextIO.putStr $ Text.take (width * 2) (Text.takeWhile (/= '\n') message) <> "\r\n"
  hFlush stdout
