module Critters.Run (run) where

import Critters.CLI qualified as CLI
import Critters.CLI.Options qualified as CLIOptions
import Critters.Command qualified as Command
import Critters.Engine qualified as Engine
import Critters.Event (Event)
import Critters.Interface (Interface)
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
loadHistory path = do
  exists <- doesPathExist path
  if exists
    then
      Save.load path >>= \case
        Left err -> die $ "Cannot load save file '" <> from path <> "': " <> err
        Right history -> pure history
    else writeHistory path [] >> pure []

loop :: Interface -> FilePath -> [Event] -> GameState -> IO ()
loop interface saveFile history state = do
  userInput <- Interface.input interface
  let command = Engine.commandFor state userInput
      (nextState, events) = Engine.step state userInput
  Interface.log interface $ "Input: " <> pretty userInput
  Interface.log interface $ "Command: " <> maybe "Nothing" pretty command
  mapM_ (\event -> Interface.log interface $ "Event: " <> pretty event) events
  nextHistory <- saveEvents saveFile history events
  Interface.output interface nextState
  case command of
    Just Command.Quit -> exitSuccess
    _ -> loop interface saveFile nextHistory nextState

run :: IO ()
run = do
  configuration <- Options.execParser CLI.options
  let gameFile = CLIOptions.gameFile configuration
      interface = Interface.select (CLIOptions.interfaceType configuration)
      saveFile = CLIOptions.saveFile configuration
  state <- loadGameState gameFile
  history <- loadHistory saveFile
  let resumedState = Save.apply state history
  Interface.log interface $ "Loaded game from " <> from gameFile
  Interface.log interface $ from $ toLazyText $ "Replayed events: " <> decimal (length history)
  Interface.output interface resumedState
  loop interface saveFile history resumedState

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
