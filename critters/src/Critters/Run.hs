module Critters.Run (run) where

import Control.Monad (forever)
import Critters.CLI qualified as CLI
import Critters.Input (Input)
import Critters.Input qualified as Input
import Critters.State.GameState (GameState)
import Data.Aeson qualified as Aeson
import Options.Applicative qualified as Options
import System.Exit (die, exitSuccess)
import System.IO (isEOF)
import System.IO.Error (ioeGetErrorString, tryIOError)

loadGameState :: FilePath -> IO GameState
loadGameState gameFile =
  tryIOError (Aeson.eitherDecodeFileStrict gameFile) >>= \case
    Left err -> die $ "Cannot read game file '" <> gameFile <> "': " <> ioeGetErrorString err
    Right (Left err) -> die $ "Invalid game file '" <> gameFile <> "': " <> err
    Right (Right state) -> pure state

readInput :: IO Input
readInput =
  isEOF >>= \case
    True -> exitSuccess
    False ->
      Input.parse <$> getLine >>= \case
        Just input -> pure input
        Nothing -> putStrLn "Unrecognized input." >> readInput

run :: IO ()
run = do
  gameFile <- Options.execParser CLI.options
  _ <- loadGameState gameFile
  putStrLn $ "Loaded game from " <> gameFile
  forever readInput
