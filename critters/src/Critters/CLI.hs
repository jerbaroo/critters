module Critters.CLI (options) where

import Critters.CLI.Options (Options (..))
import Critters.InterfaceType (InterfaceType (Debug, TUI))
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
            "tui" -> Right TUI
            _ -> Left "Expected interface: debug or tui"
        )
        $ Options.completeWith ["debug", "tui"]
            <> Options.help "Interface to use (debug or tui)"
            <> Options.long "interface"
            <> Options.metavar "INTERFACE"

options :: Options.ParserInfo Options
options =
    Options.info
        (Options.helper <*> (Options <$> gameFileParser <*> interfaceParser <*> saveNameParser))
        (Options.fullDesc <> Options.progDesc "Load a game from a JSON file.")

saveNameParser :: Options.Parser Text
saveNameParser =
    Options.strOption $
        Options.action "name"
            <> Options.help "Name of save file (next to game file)."
            <> Options.long "save"
            <> Options.metavar "SAVE_NAME"
