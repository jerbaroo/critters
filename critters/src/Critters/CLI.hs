module Critters.CLI (options) where

import Options.Applicative qualified as Options

gameFileParser :: Options.Parser FilePath
gameFileParser =
  Options.strArgument $
    Options.action "file"
      <> Options.help "Game state JSON file"
      <> Options.metavar "GAME_FILE"

options :: Options.ParserInfo FilePath
options =
  Options.info
    (Options.helper <*> gameFileParser)
    (Options.fullDesc <> Options.progDesc "Load a game from a JSON file.")
