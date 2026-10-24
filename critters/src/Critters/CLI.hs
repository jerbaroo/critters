module Critters.CLI (options) where

import Critters.CLI.Options (Options (..))
import Critters.InterfaceType (InterfaceType (Debug))
import Data.Text (Text)
import Options.Applicative qualified as Options
import Witch (from)

gameFileParser :: Options.Parser FilePath
gameFileParser =
  Options.strArgument $
    Options.action "file"
      <> Options.help "Game state JSON file"
      <> Options.metavar "GAME_FILE"

interfaceParser :: Options.Parser InterfaceType
interfaceParser =
  Options.option
    ( Options.eitherReader $ \name -> case from name :: Text of
        "debug" -> Right Debug
        _ -> Left "Expected interface: debug"
    )
    $ Options.completeWith ["debug"]
      <> Options.help "Interface to use (debug)"
      <> Options.long "interface"
      <> Options.metavar "INTERFACE"

options :: Options.ParserInfo Options
options =
  Options.info
    (Options.helper <*> (Options <$> gameFileParser <*> interfaceParser <*> saveFileParser))
    (Options.fullDesc <> Options.progDesc "Load a game from a JSON file.")

saveFileParser :: Options.Parser FilePath
saveFileParser =
  Options.strOption $
    Options.action "file"
      <> Options.help "Resume and save event history in this JSON Lines file (created if missing)"
      <> Options.long "save"
      <> Options.metavar "SAVE_FILE"
