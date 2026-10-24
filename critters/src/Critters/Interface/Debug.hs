module Critters.Interface.Debug (input, output, writeLog) where

import Critters.Input (Input)
import Critters.Input qualified as Input
import Critters.State.GameState (GameState, conversationText)
import Data.Text (Text)
import Data.Text.IO qualified as TextIO
import System.Exit (exitSuccess)
import System.IO (hFlush, isEOF, stdout)

input :: FilePath -> IO Input
input logPath = do
  TextIO.putStr "> "
  hFlush stdout
  isEOF >>= \case
    True -> exitSuccess
    False ->
      Input.parse <$> TextIO.getLine >>= \case
        Just userInput -> pure userInput
        Nothing -> writeLog () logPath "Unrecognized input." >> input logPath

output :: () -> GameState -> IO ()
output _ = mapM_ TextIO.putStrLn . conversationText

writeLog :: () -> FilePath -> Text -> IO ()
writeLog _ logPath message = do
  TextIO.putStrLn message
  TextIO.appendFile logPath message
