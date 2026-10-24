module Critters.Interface.TUI (height, input, output, render, width, writeLog) where

import Critters.Input (Input)
import Critters.Input.Direction (Direction (..))
import Critters.Interface.TUI.Keyboard qualified as Keyboard
import Critters.State.GameState (GameState (..), conversationText, npcAt, world)
import Critters.State.Position (Position (..))
import Critters.State.Tile (Tile (..))
import Critters.State.World (World (..), tileAt)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO

-- | Editable viewport dimensions, in tiles. Each tile uses two terminal columns.
height :: Int
height = 32

width :: Int
width = 64

input :: FilePath -> IO Input
input logPath = do
  Keyboard.input >>= \case
    Just userInput -> pure userInput
    Nothing -> writeLog () logPath "Unrecognized input." >> input logPath

output :: GameState -> IO ()
output state = do
  TextIO.putStr $
    "\ESC[2J\ESC[H"
      <> Text.replace "\n" "\r\n" (render state)

-- | A fixed viewport with north at the top, padded outside the world.
render :: GameState -> Text
render state = Text.unlines (rows <> maybe [] (\line -> ["", line]) (conversationText state))
  where
    building tile marker roof position
      | tileAt (world state) position{x = x position - 1} /= Just tile
          && tileAt (world state) position{y = y position + 1} /= Just tile =
          marker
      | otherwise = roof
    cell position
      | position == playerPosition state = "🧑"
      | isJust (npcAt state position) = "🧓"
      | otherwise = case tileAt (world state) position of
          Just Door -> "🚪"
          Just Fence -> "🪵"
          Just Flowers -> "🌼"
          Just Grass -> "🟩"
          Just Ground -> "⬜"
          Just Arena -> building Arena "🥊" "🟥" position
          Just House -> building House "🏠" "🟥" position
          Just Laboratory -> building Laboratory "🔬" "⬜" position
          Just (Ledge direction) -> case direction of
            East -> "⏩"
            North -> "🔺"
            South -> "🔻"
            West -> "⏪"
          Just Shop -> building Shop "🏪" "🟦" position
          Just Path -> "🟫"
          Just Clinic -> building Clinic "🏥" "🟥" position
          Just Rock -> "🪨"
          Just Sand -> "🟨"
          Just Sign -> "🪧"
          Just Stairs -> "🪜"
          Just TallGrass -> "🌿"
          Just Tree -> "🌳"
          Just Wall -> "🧱"
          Just Water -> "🌊"
          Nothing -> "\x3000"
    origin size extent coordinate = max 0 $ min (max 0 (extent - size)) (max 0 coordinate - size `div` 2)
    originX = origin width (maximum (0 : map length worldRows)) (x (playerPosition state))
    originY = origin height (length worldRows) (y (playerPosition state))
    rows =
      [ Text.concat [cell (Position (originX + column) (originY + row)) | column <- [0 .. width - 1]]
      | row <- reverse [0 .. height - 1]
      ]
    worldRows = tiles (world state)

writeLog :: () -> FilePath -> Text -> IO ()
writeLog _ = TextIO.appendFile
