module Critters.Run (run) where

import Control.Monad (forever)
import Critters.CLI qualified as CLI
import Critters.Command qualified as Command
import Critters.Input (Input)
import Critters.Input qualified as Input
import Critters.Pretty (pretty)
import Critters.State.GameState (GameState)
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import Options.Applicative qualified as Options
import System.Exit (exitFailure, exitSuccess)
import System.IO (isEOF, stderr)
import System.IO.Error (ioeGetErrorString, tryIOError)

die :: Text -> IO a
die message = TextIO.hPutStrLn stderr message >> exitFailure

loadGameState :: FilePath -> IO GameState
loadGameState gameFile =
  tryIOError (Aeson.eitherDecodeFileStrict gameFile) >>= \case
    Left err -> die $ "Cannot read game file '" <> Text.pack gameFile <> "': " <> Text.pack (ioeGetErrorString err)
    Right (Left err) -> die $ "Invalid game file '" <> Text.pack gameFile <> "': " <> Text.pack err
    Right (Right state) -> pure state

readInput :: IO Input
readInput =
  isEOF >>= \case
    True -> exitSuccess
    False ->
      Input.parse <$> TextIO.getLine >>= \case
        Just input -> pure input
        Nothing -> TextIO.putStrLn "Unrecognized input." >> readInput

run :: IO ()
run = do
  gameFile <- Options.execParser CLI.options
  state <- loadGameState gameFile
  TextIO.putStrLn $ "Loaded game from " <> Text.pack gameFile
  forever $ do
    input <- readInput
    let command = Command.fromInput state input
    TextIO.putStrLn $ "Input: " <> pretty input
    TextIO.putStrLn $ "Command: " <> maybe "Nothing" pretty command
    case command of
      Just Command.Quit -> exitSuccess
      _ -> pure ()
