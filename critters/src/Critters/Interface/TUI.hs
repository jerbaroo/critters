module Critters.Interface.TUI (animationFrames, Context, dialogueFrames, eventMessages, healthBar, height, initialContext, input, output, outputEvent, redraw, render, renderFrame, width, writeLog) where

import Control.Applicative ((<|>))
import Control.Concurrent (threadDelay)
import Control.Monad (foldM, when)
import Critters.Battle.Common qualified as BattleCommon
import Critters.Battle.Menu qualified as BattleMenu
import Critters.Event (Event)
import Critters.Event qualified as Event
import Critters.Input (Input)
import Critters.Input qualified as Input
import Critters.Input.Direction (Direction (..))
import Critters.Interface.TUI.Cell (Cell (..))
import Critters.Interface.TUI.Cell qualified as Cell
import Critters.Interface.TUI.Keyboard qualified as Keyboard
import Critters.State.ActiveInteraction (ActiveInteraction (..))
import Critters.State.Affliction qualified as Affliction
import Critters.State.Battle qualified as Battle
import Critters.State.BattleDefinition qualified as Definition
import Critters.State.BattlePhase (BattlePhase (Introduction, ItemTargetMenu, ReplacementMenu, SwitchMenu))
import Critters.State.BattleProfile qualified as Profile
import Critters.State.Choice qualified as Choice
import Critters.State.CombatConfig qualified as Config
import Critters.State.Critter qualified as Critter
import Critters.State.GameState (GameState (..), conversationText, interactionText, npcAt, objectAt, world)
import Critters.State.GameState qualified as GameState
import Critters.State.Interaction qualified as Interaction
import Critters.State.Object qualified as Object
import Critters.State.Party qualified as Party
import Critters.State.Position (Position (..))
import Critters.State.Position qualified as Position
import Critters.State.Species qualified as Species
import Critters.State.Tile (Tile (..))
import Critters.State.World (tileAt)
import Data.Char (isControl, toUpper)
import Data.Char.WCWidth (wcwidth)
import Data.List (groupBy)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy.Builder.Int (decimal)
import System.Exit (exitSuccess)
import System.IO (hFlush, stdout)
import Witch (from, tryFrom)

-- | Display frames only; interpolated health never enters game state or saves.
animationFrames :: Int -> GameState -> GameState -> [[[Cell]]]
animationFrames = animationFramesWith (\health -> renderFrameWithHealth health Nothing)

animationFramesWith :: ([(BattleCommon.Side, Int)] -> GameState -> [[Cell]]) -> Int -> GameState -> GameState -> [[[Cell]]]
animationFramesWith renderer count previous state = case (BattleCommon.contextFor previous, BattleCommon.contextFor state) of
    (Just before, Just after)
        | before.battle.definition == after.battle.definition
        , before.battle.initialSeed == after.battle.initialSeed
        , count > 0
        , let changes =
                [ (side, old.hitPoints, new.hitPoints)
                | side <- [BattleCommon.Enemy, BattleCommon.Player]
                , BattleCommon.activeIndex side before == BattleCommon.activeIndex side after
                , Just old <- [BattleCommon.active side before]
                , Just new <- [BattleCommon.active side after]
                , old.species == new.species
                , old.maxHitPoints == new.maxHitPoints
                , old.hitPoints /= new.hitPoints
                ]
        , not (null changes) ->
            [ renderer [(side, interpolate frame old new) | (side, old, new) <- changes] state
            | frame <- [1 .. count]
            ]
    _ -> [renderer [] state]
  where
    interpolate frame old new =
        either (const new) id $
            tryFrom @Integer @Int $
                from @Int @Integer old
                    + (from @Int @Integer new - from @Int @Integer old) * from @Int @Integer frame `quot` from @Int @Integer count

awaitDialogueAdvance :: IO ()
awaitDialogueAdvance =
    Keyboard.input >>= \case
        Just Input.Enter -> Keyboard.discardInput
        Just (Input.Character 'q') -> exitSuccess
        _ -> awaitDialogueAdvance

battleMessage :: GameState -> Text -> Text
battleMessage state rawValue = case BattleCommon.contextFor state of
    Just context
        | value == "Victory." -> "You defeated " <> resolveText state context.battle.definition.name
        | otherwise -> capitalize (speciesText context.battle.config (BattleCommon.allCritters BattleCommon.Enemy context <> BattleCommon.allCritters BattleCommon.Player context) value)
    Nothing -> capitalize value
  where
    value = resolveText state rawValue

resolveText :: GameState -> Text -> Text
resolveText state value = either (error . Text.unpack) id (GameState.resolveText state value)

capitalize :: Text -> Text
capitalize value = case Text.uncons value of
    Just (first, rest) -> Text.cons (toUpper first) rest
    Nothing -> value

data Context = Context
    { previousFrame :: [[Cell]]
    , previousState :: Maybe GameState
    , seenMessages :: Int
    }

damageMessage :: Text -> Bool
damageMessage message = case Text.stripSuffix " HP." amount of
    Just digits -> not (Text.null prefix) && not (Text.null digits) && Text.all (\value -> value >= '0' && value <= '9') digits
    Nothing -> False
  where
    (prefix, amount) = Text.breakOnEnd " lost " message

-- | The move's dialogue stays visible throughout its health animation.
dialogueFrames :: Int -> Text -> GameState -> GameState -> [[[Cell]]]
dialogueFrames count message previous state =
    animationFramesWith (\health -> renderFrameWithHealth health (Just (battleMessage state message))) count previous state

drawFrames :: Context -> GameState -> [[[Cell]]] -> IO Context
drawFrames previous state frames = do
    let draw old (index, rows) = do
            when (index > (0 :: Int)) (threadDelay 30000)
            TextIO.putStr (redraw old rows)
            hFlush stdout
            pure rows
    rows <- foldM draw previous.previousFrame (zip [0 ..] frames)
    pure previous{previousFrame = rows, previousState = Just state}

-- | Consume only this event's new messages, never replaying earlier moves.
eventMessages :: Int -> Event -> GameState -> GameState -> [Text]
eventMessages seen event previous state = case event of
    Event.BattleCombatantsSentOut -> case BattleCommon.contextFor state of
        Just context ->
            [ case side of
                BattleCommon.Enemy -> context.battle.definition.name <> " sent out " <> name <> "."
                BattleCommon.Player -> "Go, " <> name <> "!"
            | side <- [BattleCommon.Enemy, BattleCommon.Player]
            , Just critter <- [BattleCommon.active side context]
            , let name = speciesName context.battle.config critter
            ]
        Nothing -> []
    _ ->
        if presents
            then map (battleMessage state) (filter (\message -> not (Text.null message) && not (damageMessage message)) (drop seen current))
            else []
  where
    current = maybe [] (.messages) state.battle
    presents = case event of
        Event.BattleChanged _ -> current /= maybe [] (.messages) previous.battle
        Event.MoveUsed _ _ _ -> True
        Event.TurnCompleted _ -> True
        _ -> False

-- | A fixed-width health gauge, filled in proportion to current health.
healthBar :: Int -> Int -> Text
healthBar current maximumHealth = "[" <> Text.replicate filled "█" <> Text.replicate (16 - filled) "░" <> "]"
  where
    filled
        | maximumHealth <= 0 = 0
        | otherwise = length [segment | segment <- [1 .. 16 :: Int], from @Int @Integer segment * from @Int @Integer maximumHealth <= from @Int @Integer current * 16]

-- | Editable viewport dimensions, in tiles. Each tile uses two terminal columns.
height :: Int
height = 24

initialContext :: Context
initialContext = Context [] Nothing 0

input :: FilePath -> IO Input
input logPath = do
    Keyboard.input >>= \case
        Just userInput -> pure userInput
        Nothing -> writeLog initialContext logPath "Unrecognized input." >> input logPath

output :: Context -> GameState -> IO Context
output previous state = do
    let frames = maybe [renderFrame state] (\old -> animationFrames 12 old state) previous.previousState
    context <- drawFrames previous state frames
    pure context{seenMessages = 0}

outputEvent :: Context -> Event -> GameState -> IO Context
outputEvent previous event state = do
    let old = maybe state id previous.previousState
        messages = eventMessages previous.seenMessages event old state
        play context message = do
            let frames = case event of
                    Event.BattleCombatantsSentOut -> [renderIntroduction message state]
                    _ -> dialogueFrames 12 message (maybe state id context.previousState) state
            played <- drawFrames context state frames
            case event of
                Event.MoveUsed _ _ _ -> do
                    threadDelay (max 0 (1000000 - (length frames - 1) * 30000))
                    Keyboard.discardInput
                _ -> do
                    when (length frames > 1) Keyboard.discardInput
                    when (not (Text.null message)) awaitDialogueAdvance
            pure played
    played <- case event of
        Event.NpcMoved _ _ -> do
            context <- drawFrames previous state [renderFrame state{GameState.activeInteraction = Nothing, GameState.conversation = Nothing}]
            threadDelay 540000
            Keyboard.discardInput
            pure context
        _ -> case messages of
            []
                | length (animationFrames 12 old state) > 1 -> play previous ""
                | otherwise -> pure previous
            _ -> foldM play previous messages
    let seen = case event of
            Event.BattleChanged _ -> maybe 0 (length . (.messages)) state.battle
            Event.MoveUsed _ _ _ -> maybe 0 (length . (.messages)) state.battle
            Event.TurnCompleted _ -> maybe 0 (length . (.messages)) state.battle
            _ -> previous.seenMessages
    pure played{previousState = Just state, seenMessages = seen}

{- | Replace changed cells using absolute cursor positions. An empty previous
frame means this is the first draw, so clear the screen once.
-}
redraw :: [[Cell]] -> [[Cell]] -> Text
redraw [] rows = "\ESC[0m\ESC[2J\ESC[H" <> Text.intercalate "\r\n" (map renderRow rows)
redraw previous rows =
    Text.concat
        [ "\ESC[" <> number row <> ";" <> number column <> "H" <> renderRow [cell]
        | (row, (oldRow, cells)) <- zip [1 :: Int ..] (zip (previous <> repeat []) rows)
        , let old = Map.fromList (positioned oldRow)
        , (column, cell) <- positioned cells
        , Map.lookup column old /= Just cell
        ]
  where
    number value = from (toLazyText (decimal value))
    positioned cells = zip (scanl (\column cell -> column + Cell.columns cell) 1 cells) cells

render :: GameState -> Text
render = Text.intercalate "\n" . map renderRow . renderFrame

renderBattleWithHealth :: [(BattleCommon.Side, Int)] -> Maybe Text -> GameState -> [[Cell]]
renderBattleWithHealth health dialogue state = case BattleCommon.contextFor state of
    Nothing -> renderText height ""
    Just context
        | context.battle.phase == Introduction ->
            let active = context.battle
                dialogueLines = active.definition.introduction
                message = case drop active.introductionIndex dialogueLines of
                    line : _ -> line
                    [] -> ""
             in renderIntroduction (maybe message id dialogue) state
    Just context ->
        case dialogue of
            Just message -> take (height - 4) (details <> renderText height "") <> renderDialogue message
            Nothing
                | partySelection -> details <> concatMap (renderText 1) (take menuHeight (drop offset choices <> repeat ""))
                | otherwise -> take (height - length options) (details <> renderText height "") <> options
      where
        active = context.battle
        choices = [(if index == active.selected then "> " else "  ") <> label | (index, label) <- zip [0 ..] labels]
        critters = BattleCommon.allCritters BattleCommon.Player context
        details =
            renderText 6 (Text.intercalate "\n" [resolveText state active.definition.name, "", description BattleCommon.Enemy context, gauge BattleCommon.Enemy context, description BattleCommon.Player context, gauge BattleCommon.Player context])
                <> renderText 5 ""
        labels = map (resolveText state . (if partySelection then speciesText active.config critters else id) . fst) (BattleMenu.entries state)
        menuHeight = height - length details
        offset = max 0 (active.selected - menuHeight + 1)
        options = renderOptions active.selected labels
        partySelection = active.phase `elem` [ItemTargetMenu, ReplacementMenu, SwitchMenu]
  where
    description side context = case BattleCommon.active side context of
        Nothing -> "—"
        Just critter -> speciesName context.battle.config critter <> "  Lv " <> number critter.level <> "  HP " <> number (hitPoints side critter) <> "/" <> number critter.maxHitPoints <> maybe "" (\status -> "  [" <> status.status <> "]") critter.battleProfile.status
    gauge side context = maybe "" (\critter -> healthBar (hitPoints side critter) critter.maxHitPoints) (BattleCommon.active side context)
    hitPoints side critter = maybe critter.hitPoints id (lookup side health)
    number value = from (toLazyText (decimal value))

renderChoices :: GameState -> ActiveInteraction -> [[Cell]]
renderChoices state active =
    renderText (height - length options) question <> options
  where
    options = renderOptions (maybe (-1) id active.selectedChoice) (map (resolveText state . (.label)) active.script.choices)
    question = case drop active.dialogueIndex active.script.dialogue of
        line : _ | active.dialogueIndex >= 0 -> resolveText state line
        _ -> ""

-- | Two padded text rows, clipped to the viewport's column width.
renderConversation :: Text -> [[Cell]]
renderConversation = renderText 2 . wrapText (width * 2)

renderDialogue :: Text -> [[Cell]]
renderDialogue message =
    border "┌" "┐" : map row (renderTextColumns (width * 2 - 4) 2 (wrapText (width * 2 - 4) message)) <> [border "└" "┘"]
  where
    border left right = concat (renderText 1 (left <> Text.replicate (width * 2 - 2) "─" <> right))
    row cells = [Cell "│" True, Cell " " True] <> cells <> [Cell " " True, Cell "│" True]

{- | A fixed viewport centered on the player, with north at the top and blanks
for missing cells, including those outside the map.
-}
renderFrame :: GameState -> [[Cell]]
renderFrame = renderFrameWithHealth [] Nothing

renderFrameWithHealth :: [(BattleCommon.Side, Int)] -> Maybe Text -> GameState -> [[Cell]]
renderFrameWithHealth health dialogue state
    | state.partyOpen = renderParty state
    | isJust state.battle = renderBattleWithHealth health dialogue state
    | Just active <- state.activeInteraction, isJust active.selectedChoice = renderChoices state active
    | otherwise = case interactionText state <|> conversationText state of
        Just message -> take (height - 2) rows <> renderConversation (resolveText state message)
        Nothing -> rows
  where
    building tile symbol roof position
        | tileAt (world state) position{Position.x = position.x - 1} /= Just tile
            && tileAt (world state) position{Position.y = position.y + 1} /= Just tile =
            symbol
        | otherwise = roof
    cell position = Cell (marker position) False
    marker position
        | position == state.playerPosition = "🧑"
        | isJust (npcAt state position) = "🧓"
        | Just (_, object) <- objectAt state position = object.glyph
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
    originX = state.playerPosition.x - width `div` 2
    originY = state.playerPosition.y + height `div` 2
    rows =
        [ [cell (Position (originX + column) (originY - row)) | column <- [0 .. width - 1]]
        | row <- [0 .. height - 1]
        ]

-- | Introduction and send-out dialogue precede combatant details and options.
renderIntroduction :: Text -> GameState -> [[Cell]]
renderIntroduction message state =
    renderText (height - 4) heading <> renderDialogue (resolveText state message)
  where
    heading = maybe "" (resolveText state . (.definition.name)) state.battle

-- | Two options per row, with at least two rows and no upper row limit.
renderOptions :: Int -> [Text] -> [[Cell]]
renderOptions selected labels =
    border "┌" "┐" : pairs (entries <> replicate (4 - length labels) "") <> [border "└" "┘"]
  where
    border left right = concat (renderText 1 (left <> Text.replicate (width * 2 - 2) "─" <> right))
    entries = [(if index == selected then " > " else "   ") <> label | (index, label) <- zip [0 ..] labels]
    option label = concat (renderTextColumns (width - 1) 1 label)
    pairs [] = []
    pairs [left] = [row left ""]
    pairs (left : right : rest) = row left right : pairs rest
    row left right = [Cell "│" True] <> option left <> option right <> [Cell "│" True]

-- | Display the party within the viewport.
renderParty :: GameState -> [[Cell]]
renderParty state =
    renderText height (Text.intercalate "\n" (heading : "" : entries))
  where
    critters = state.party.members
    entries = case critters of
        [] -> ["Your party is empty"]
        _ -> zipWith entry [1 ..] critters
    entry index critter =
        number index
            <> ". "
            <> speciesName state.combat critter
            <> "  Lv "
            <> number critter.level
            <> "  HP "
            <> number critter.hitPoints
            <> "/"
            <> number critter.maxHitPoints
    heading = "Party — " <> number (length critters) <> " / " <> number state.maxPartySize
    number :: Int -> Text
    number value = from (toLazyText (decimal value))

renderRow :: [Cell] -> Text
renderRow [] = ""
renderRow (first : rest) =
    let (same, remaining) = span ((== first.whiteBackground) . (.whiteBackground)) rest
        text = Text.concat (map (.glyph) (first : same))
     in (if first.whiteBackground then "\ESC[30;107m" <> text <> "\ESC[0m" else text) <> renderRow remaining

-- | Wrap and pad text to the viewport's column width, clipping extra rows.
renderText :: Int -> Text -> [[Cell]]
renderText = renderTextColumns (width * 2)

renderTextColumns :: Int -> Int -> Text -> [[Cell]]
renderTextColumns columns rowCount message = map row $ take rowCount (wrapped <> repeat "")
  where
    plain = Text.filter (\value -> value == '\n' || not (isControl value)) message
    row line =
        [ cell
        | characters <- groupBy (\_ value -> wcwidth value == 0) (from (line <> Text.replicate (columns - textWidth line) " "))
        , let cell = Cell (from characters) True
        , Cell.columns cell > 0
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

speciesName :: Config.CombatConfig -> Critter.Critter -> Text
speciesName config critter = case Map.lookup identifier config.species of
    Just definition | not (Text.null definition.name) -> definition.name
    _ -> capitalize identifier
  where
    identifier = critter.species

speciesText :: Config.CombatConfig -> [Critter.Critter] -> Text -> Text
speciesText config critters message = foldr replace message critters
  where
    replace critter value = case Text.stripPrefix (critter.species <> " ") value of
        Just rest -> speciesName config critter <> " " <> rest
        Nothing -> value

width :: Int
width = 32

wrapText :: Int -> Text -> Text
wrapText columns = Text.intercalate "\n" . concatMap wrapLine . Text.splitOn "\n"
  where
    addWord rows word = case rows of
        [] -> [word]
        line : previous
            | textWidth line + 1 + textWidth word <= columns -> (line <> " " <> word) : previous
            | otherwise -> word : line : previous
    textWidth = Text.foldl' (\total value -> total + max 0 (wcwidth value)) 0
    wrapLine line = case Text.words line of
        [] -> [""]
        wordsInLine -> reverse (foldl addWord [] wordsInLine)

writeLog :: Context -> FilePath -> Text -> IO ()
writeLog _ logPath message = TextIO.appendFile logPath (message <> "\n")
