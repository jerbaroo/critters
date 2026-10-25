module Critters.Interface.Debug (input, output, writeLog) where

import Control.Applicative ((<|>))
import Control.Monad (forM_)
import Critters.Battle.Common qualified as BattleCommon
import Critters.Battle.Menu qualified as BattleMenu
import Critters.Input (Input)
import Critters.Input qualified as Input
import Critters.State.ActiveInteraction qualified as Active
import Critters.State.Battle qualified as Battle
import Critters.State.BattleDefinition qualified as Definition
import Critters.State.Choice qualified as Choice
import Critters.State.Critter qualified as Critter
import Critters.State.GameState (GameState (..), conversationText, interactionText)
import Critters.State.GameState qualified as GameState
import Critters.State.Interaction qualified as Interaction
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import System.Exit (exitSuccess)
import System.IO (hFlush, isEOF, stdout)
import Witch (from)

input :: FilePath -> IO Input
input logPath = do
    TextIO.putStr "> "
    hFlush stdout
    isEOF >>= \case
        True -> exitSuccess
        False ->
            Input.parse <$> TextIO.getLine >>= \case
                Just userInput -> pure userInput
                Nothing -> writeLog () logPath "Unrecognized input." >> input logPath

output :: () -> GameState -> IO ()
output _ state = do
    forM_ (BattleCommon.contextFor state) $ \context -> do
        let active = context.battle
        TextIO.putStrLn ("Battle: " <> resolveText state active.definition.name)
        forM_ [BattleCommon.Enemy, BattleCommon.Player] $ \side ->
            forM_ (BattleCommon.active side context) $ \critter -> TextIO.putStrLn (BattleCommon.displayName context critter <> " HP " <> from (show critter.hitPoints) <> "/" <> from (show critter.maxHitPoints))
        mapM_ (TextIO.putStrLn . resolveText state) active.messages
        forM_ (zip [0 ..] (BattleMenu.entries state)) $ \(index, (label, _)) -> TextIO.putStrLn ((if index == active.selected then "> " else "  ") <> resolveText state label)
    mapM_ (TextIO.putStrLn . resolveText state) (interactionText state <|> conversationText state)
    forM_ state.activeInteraction $ \active ->
        case active.selectedChoice of
            Just selected ->
                forM_ (zip [0 ..] active.script.choices) $ \(index, choice) ->
                    TextIO.putStrLn ((if index == selected then "> " else "  ") <> resolveText state choice.label)
            Nothing -> pure ()

resolveText :: GameState -> Text -> Text
resolveText state value = either (error . Text.unpack) id (GameState.resolveText state value)

writeLog :: () -> FilePath -> Text -> IO ()
writeLog _ logPath message = do
    TextIO.putStrLn message
    TextIO.appendFile logPath (message <> "\n")
