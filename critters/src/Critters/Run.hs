module Critters.Run (run) where

import Critters.CLI qualified as CLI
import Critters.CLI.Options qualified as CLIOptions
import Critters.Command qualified as Command
import Critters.Engine qualified as Engine
import Critters.Event (Event)
import Critters.Input (Input)
import Critters.Input qualified as Input
import Critters.Pretty (pretty)
import Critters.Save qualified as Save
import Critters.State.GameState (GameState)
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Data.Text.IO qualified as TextIO
import Options.Applicative qualified as Options
import System.Directory (doesPathExist)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hFlush, isEOF, stderr, stdout)
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

loop :: FilePath -> [Event] -> GameState -> IO ()
loop saveFile history state = do
  input <- readInput
  let command = Engine.commandFor state input
      (nextState, events) = Engine.step state input
  TextIO.putStrLn $ "Input: " <> pretty input
  TextIO.putStrLn $ "Command: " <> maybe "Nothing" pretty command
  nextHistory <- saveEvents saveFile history events
  case command of
    Just Command.Quit -> exitSuccess
    _ -> loop saveFile nextHistory nextState

readInput :: IO Input
readInput = do
  TextIO.putStr "> "
  hFlush stdout
  isEOF >>= \case
    True -> exitSuccess
    False ->
      Input.parse <$> TextIO.getLine >>= \case
        Just input -> pure input
        Nothing -> TextIO.putStrLn "Unrecognized input." >> readInput

run :: IO ()
run = do
  configuration <- Options.execParser CLI.options
  let gameFile = CLIOptions.gameFile configuration
      saveFile = CLIOptions.saveFile configuration
  state <- loadGameState gameFile
  history <- loadHistory saveFile
  TextIO.putStrLn $ "Loaded game from " <> from gameFile
  loop saveFile history (Save.apply state history)

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
