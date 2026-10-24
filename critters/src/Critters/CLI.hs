module Critters.CLI (run) where

import Critters.State.GameState (GameState)
import Data.Aeson qualified as Aeson
import Options.Applicative qualified as Options
import System.Exit (die)
import System.IO.Error (ioeGetErrorString, tryIOError)

gameFileParser :: Options.Parser FilePath
gameFileParser =
  Options.strArgument
    ( Options.action "file"
        <> Options.help "Game state JSON file"
        <> Options.metavar "GAME_FILE"
    )

loadGameState :: FilePath -> IO GameState
loadGameState gameFile = do
  result <- tryIOError (Aeson.eitherDecodeFileStrict gameFile)
  case result of
    Left err -> die ("Cannot read game file '" ++ gameFile ++ "': " ++ ioeGetErrorString err)
    Right (Left err) -> die ("Invalid game file '" ++ gameFile ++ "': " ++ err)
    Right (Right state) -> pure state

options :: Options.ParserInfo FilePath
options =
  Options.info
    (Options.helper <*> gameFileParser)
    (Options.fullDesc <> Options.progDesc "Load a game from a JSON file.")

run :: IO ()
run = do
  gameFile <- Options.execParser options
  _ <- loadGameState gameFile
  putStrLn $ "Loaded game from " <> gameFile
