module Critters.CLI (options) where

import Critters.CLI.Options (Options (..))
import Options.Applicative qualified as Options

gameFileParser :: Options.Parser FilePath
gameFileParser =
  Options.strArgument $
    Options.action "file"
      <> Options.help "Game state JSON file"
      <> Options.metavar "GAME_FILE"

options :: Options.ParserInfo Options
options =
  Options.info
    (Options.helper <*> (Options <$> gameFileParser <*> saveFileParser))
    (Options.fullDesc <> Options.progDesc "Load a game from a JSON file.")

saveFileParser :: Options.Parser FilePath
saveFileParser =
  Options.strOption $
    Options.action "file"
      <> Options.help "Resume and save event history in this JSON Lines file (created if missing)"
      <> Options.long "save"
      <> Options.metavar "SAVE_FILE"
