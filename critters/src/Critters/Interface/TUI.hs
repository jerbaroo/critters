module Critters.Interface.TUI (height, input, output, redraw, render, renderFrame, width, writeLog) where

import Control.Applicative ((<|>))
import Critters.Battle.Common qualified as BattleCommon
import Critters.Battle.Menu qualified as BattleMenu
import Critters.Input (Input)
import Critters.Input.Direction (Direction (..))
import Critters.Interface.TUI.Cell (Cell (..))
import Critters.Interface.TUI.Cell qualified as Cell
import Critters.Interface.TUI.Keyboard qualified as Keyboard
import Critters.State.ActiveInteraction (ActiveInteraction (..))
import Critters.State.Affliction qualified as Affliction
import Critters.State.Battle qualified as Battle
import Critters.State.BattleDefinition qualified as Definition
import Critters.State.BattlePhase (BattlePhase (..))
import Critters.State.BattleProfile qualified as Profile
import Critters.State.Choice qualified as Choice
import Critters.State.Critter qualified as Critter
import Critters.State.GameState (GameState (..), conversationText, interactionText, npcAt, objectAt, world)
import Critters.State.Interaction qualified as Interaction
import Critters.State.Object qualified as Object
import Critters.State.Party qualified as Party
import Critters.State.Position (Position (..))
import Critters.State.Tile (Tile (..))
import Critters.State.World (tileAt)
import Data.Char (isControl)
import Data.Char.WCWidth (wcwidth)
import Data.List (groupBy)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy.Builder.Int (decimal)
import System.IO (hFlush, stdout)
import Witch (from)

-- | Editable viewport dimensions, in tiles. Each tile uses two terminal columns.
height :: Int
height = 24

width :: Int
width = 32

input :: FilePath -> IO Input
input logPath = do
  Keyboard.input >>= \case
    Just userInput -> pure userInput
    Nothing -> writeLog [] logPath "Unrecognized input." >> input logPath

output :: [[Cell]] -> GameState -> IO [[Cell]]
output previous state = do
  let rows = renderFrame state
  TextIO.putStr (redraw previous rows)
  hFlush stdout
  pure rows

-- | Replace changed cells using absolute cursor positions. An empty previous
-- frame means this is the first draw, so clear the screen once.
redraw :: [[Cell]] -> [[Cell]] -> Text
redraw [] rows = "\ESC[0m\ESC[2J\ESC[H" <> Text.intercalate "\r\n" (map renderRow rows)
redraw previous rows =
  Text.concat
    [ "\ESC[" <> number row <> ";" <> number column <> "H" <> renderRow [cell]
    | (row, (oldRow, cells)) <- zip [1 :: Int ..] (zip (previous <> repeat []) rows),
      let old = Map.fromList (positioned oldRow),
      (column, cell) <- positioned cells,
      Map.lookup column old /= Just cell
    ]
  where
    number value = from (toLazyText (decimal value))
    positioned cells = zip (scanl (\column cell -> column + Cell.columns cell) 1 cells) cells

render :: GameState -> Text
render = Text.intercalate "\n" . map renderRow . renderFrame

renderBattle :: GameState -> [[Cell]]
renderBattle state = case BattleCommon.contextFor state of
  Nothing -> renderText height ""
  Just context ->
    renderText 6 (Text.intercalate "\n" ["Battle — " <> Definition.definitionName (Battle.battleDefinition active), "", description "Opponent" BattleCommon.Enemy context, description "You" BattleCommon.Player context, "", ""])
      <> renderText 5 (Text.intercalate "\n" (reverse (take 5 (reverse (Battle.battleMessages active)))))
      <> renderText 1 (case Battle.battlePhase active of Finished -> "Battle complete:"; MoveMenu -> "Choose a move:"; ItemMenu -> "Choose an item:"; ItemTargetMenu -> "Choose a target:"; ReplacementMenu -> "Choose a replacement:"; SwitchMenu -> "Choose a critter:"; ActionMenu -> "Choose an action:")
      <> concatMap (renderText 1) (take menuHeight (drop offset choices <> repeat ""))
      <> renderText 1 (case Battle.battlePhase active of Finished -> "Enter: Continue  q: Quit"; ReplacementMenu -> "↑/↓ j/k: Select  Enter: Switch  q: Quit"; _ -> "↑/↓ j/k: Select  Enter: Confirm  Esc: Back  q: Quit")
    where
      active = BattleCommon.contextBattle context
      choices = [(if index == Battle.battleSelected active then "> " else "  ") <> label | (index, (label, _)) <- zip [0 ..] (BattleMenu.entries state)]
      menuHeight = height - 13
      offset = max 0 (Battle.battleSelected active - menuHeight + 1)
  where
    description label side context = case BattleCommon.active side context of
      Nothing -> label <> ": —"
      Just critter -> label <> ": " <> BattleCommon.displayName context critter <> "  Lv " <> number (Critter.level critter) <> "  HP " <> number (Critter.hitPoints critter) <> "/" <> number (Critter.maxHitPoints critter) <> maybe "" (\status -> "  [" <> Affliction.afflictionStatus status <> "]") (Profile.profileStatus (Critter.battleProfile critter))
    number value = from (toLazyText (decimal value))

renderChoices :: ActiveInteraction -> [[Cell]]
renderChoices active =
  renderText height (Text.intercalate "\n" (question : "" : entries))
  where
    entries =
      [ (if activeSelectedChoice active == Just index then "> " else "  ") <> Choice.choiceLabel choice
      | (index, choice) <- zip [0 ..] (Interaction.interactionChoices (activeScript active))
      ]
    question = case drop (activeDialogueIndex active) (Interaction.interactionDialogue (activeScript active)) of
      line : _ | activeDialogueIndex active >= 0 -> line
      _ -> "Choose:"

-- | Two padded text rows, clipped to the viewport's column width.
renderConversation :: Text -> [[Cell]]
renderConversation = renderText 2 . wrapDialogue
  where
    columns = width * 2
    textWidth = Text.foldl' (\total value -> total + max 0 (wcwidth value)) 0
    addWord rows word = case rows of
      [] -> [word]
      line : previous
        | textWidth line + 1 + textWidth word <= columns -> (line <> " " <> word) : previous
        | otherwise -> word : line : previous
    wrapLine line = case Text.words line of
      [] -> [""]
      wordsInLine -> reverse (foldl addWord [] wordsInLine)
    wrapDialogue = Text.intercalate "\n" . concatMap wrapLine . Text.splitOn "\n"

-- | A fixed viewport centered on the player, with north at the top and blanks
-- for missing cells, including those outside the map.
renderFrame :: GameState -> [[Cell]]
renderFrame state
  | partyOpen state = renderParty state
  | isJust (battle state) = renderBattle state
  | Just active <- activeInteraction state, isJust (activeSelectedChoice active) = renderChoices active
  | otherwise = case interactionText state <|> conversationText state of
      Just message -> take (height - 2) rows <> renderConversation message
      Nothing -> rows
  where
    building tile symbol roof position
      | tileAt (world state) position{x = x position - 1} /= Just tile
          && tileAt (world state) position{y = y position + 1} /= Just tile =
          symbol
      | otherwise = roof
    cell position = Cell (marker position) False
    marker position
      | position == playerPosition state = "🧑"
      | isJust (npcAt state position) = "🧓"
      | Just (_, object) <- objectAt state position = Object.objectGlyph object
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
    originX = x (playerPosition state) - width `div` 2
    originY = y (playerPosition state) + height `div` 2
    rows =
      [ [cell (Position (originX + column) (originY - row)) | column <- [0 .. width - 1]]
      | row <- [0 .. height - 1]
      ]

-- | Display the party within the viewport, reserving the last row for closing.
renderParty :: GameState -> [[Cell]]
renderParty state =
  renderText (height - 1) (Text.intercalate "\n" (heading : "" : entries))
    <> renderText 1 "Esc: Close"
  where
    critters = Party.members (party state)
    entries = case critters of
      [] -> ["Your party is empty"]
      _ -> zipWith entry [1 ..] critters
    entry index critter =
      number index
        <> ". "
        <> Critter.species critter
        <> "  Lv "
        <> number (Critter.level critter)
        <> "  HP "
        <> number (Critter.hitPoints critter)
        <> "/"
        <> number (Critter.maxHitPoints critter)
    heading = "Party — " <> number (length critters) <> " / " <> number (maxPartySize state)
    number :: Int -> Text
    number value = from (toLazyText (decimal value))

renderRow :: [Cell] -> Text
renderRow [] = ""
renderRow (first : rest) =
  let (same, remaining) = span ((== whiteBackground first) . whiteBackground) rest
      text = Text.concat (map glyph (first : same))
   in (if whiteBackground first then "\ESC[30;107m" <> text <> "\ESC[0m" else text) <> renderRow remaining

-- | Wrap and pad text to the viewport's column width, clipping extra rows.
renderText :: Int -> Text -> [[Cell]]
renderText rowCount message = map row $ take rowCount (wrapped <> repeat "")
  where
    columns = width * 2
    plain = Text.filter (\value -> value == '\n' || not (isControl value)) message
    row line =
      [ cell
      | characters <- groupBy (\_ value -> wcwidth value == 0) (from (line <> Text.replicate (columns - textWidth line) " ")),
        let cell = Cell (from characters) True,
        Cell.columns cell > 0
      ]
    split remaining line remainingText = case Text.uncons remainingText of
      Nothing -> (line, "")
      Just (value, rest)
        | characterWidth > remaining -> (line, remainingText)
        | otherwise -> split (remaining - characterWidth) (line <> from [value]) rest
        where
          characterWidth = max 0 (wcwidth value)
    textWidth = Text.foldl' (\total value -> total + max 0 (wcwidth value)) 0
    wrap remainingText
      | Text.null remainingText = [""]
      | otherwise =
          let (line, rest) = split columns "" remainingText
           in line : if Text.null rest then [] else wrap rest
    wrapped = concatMap wrap (Text.splitOn "\n" plain)

writeLog :: [[Cell]] -> FilePath -> Text -> IO ()
writeLog _ = TextIO.appendFile
