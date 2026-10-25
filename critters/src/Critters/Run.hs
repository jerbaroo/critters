module Critters.Run (playEvents, run) where

import Control.Monad (foldM)
import Critters.CLI qualified as CLI
import Critters.CLI.Options qualified as CLIOptions
import Critters.Command qualified as Command
import Critters.Engine qualified as Engine
import Critters.Event (Event)
import Critters.Interface (Interface (..))
import Critters.Interface qualified as Interface
import Critters.Pretty (pretty)
import Critters.Save qualified as Save
import Critters.State.GameState (GameState)
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Data.Text.IO qualified as TextIO
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy.Builder.Int (decimal)
import Options.Applicative qualified as Options
import System.Directory (doesPathExist)
import System.Exit (exitFailure, exitSuccess)
import System.IO (stderr)
import System.IO.Error (ioeGetErrorString, tryIOError)
import Witch (from)

die :: Text -> IO a
die message = TextIO.hPutStrLn stderr message >> exitFailure

loadGameState :: FilePath -> IO GameState
loadGameState gameFile =
  tryIOError (Aeson.eitherDecodeFileStrict gameFile) >>= \case
    Left err -> die $ "Cannot read game file '" <> from gameFile <> "': " <> from (ioeGetErrorString err)
    Right (Left err) -> die $ "Invalid game file '" <> from gameFile <> "': " <> from err
    Right (Right state) -> pure state

loadHistory :: FilePath -> IO [Event]
loadHistory path =
  doesPathExist path >>= \case
    True ->
      Save.load path >>= \case
        Left err -> die $ "Cannot load save file '" <> from path <> "': " <> err
        Right history -> pure history
    False -> writeHistory path [] >> pure []

loop :: FilePath -> Interface a -> a -> FilePath -> [Event] -> GameState -> IO ()
loop logPath interface context saveFile history state = do
  userInput <- interface.input logPath
  let command = Engine.commandFor state userInput
      events = maybe [] (Engine.eventsFor state) command
  nextHistory <- saveEvents saveFile history events
  (playedContext, nextState) <- playEvents interface context state events
  nextContext <- interface.output playedContext nextState
  interface.log nextContext logPath $ "Input: " <> pretty userInput
  interface.log nextContext logPath $ "Command: " <> maybe "Nothing" pretty command
  mapM_ (\event -> interface.log nextContext logPath $ "Event: " <> pretty event) events
  case command of
    Just Command.Quit -> exitSuccess
    _ -> loop logPath interface nextContext saveFile nextHistory nextState

-- | Apply recorded facts in order, allowing the interface to finish each one.
playEvents :: Interface a -> a -> GameState -> [Event] -> IO (a, GameState)
playEvents interface context state = foldM apply (context, state)
  where
    apply (previousContext, previousState) event = do
      let nextState = Engine.applyEvent previousState event
      nextContext <- interface.outputEvent previousContext event nextState
      pure (nextContext, nextState)

run :: IO ()
run = do
  configuration <- Options.execParser CLI.options
  let gameFile = CLIOptions.gameFile configuration
      saveName = CLIOptions.saveName configuration
      saveFile = gameFile <> "_save_" <> from saveName
      logPath = saveFile <> "_logs"
  state <- loadGameState gameFile
  history <- loadHistory saveFile
  let resumedState = Save.apply state history
  Interface.select (CLIOptions.interfaceType configuration) $ \interface initialContext -> do
    context <- Interface.output interface initialContext resumedState
    interface.log context logPath $ "Loaded game from " <> from gameFile
    interface.log context logPath $ from $ toLazyText $ "Replayed events: " <> decimal (length history)
    loop logPath interface context saveFile history resumedState

saveEvents :: FilePath -> [Event] -> [Event] -> IO [Event]
saveEvents _ history [] = pure history
saveEvents path history events = do
  let nextHistory = history <> events
  writeHistory path nextHistory
  pure nextHistory

writeHistory :: FilePath -> [Event] -> IO ()
writeHistory path history =
  Save.save path history >>= \case
    Left err -> die $ "Cannot write save file '" <> from path <> "': " <> err
    Right () -> pure ()
