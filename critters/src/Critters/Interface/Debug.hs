module Critters.Interface.Debug (input, output, writeLog) where

import Critters.Input (Input)
import Critters.Input qualified as Input
import Critters.State.GameState (GameState)
import Data.Text (Text)
import Data.Text.IO qualified as TextIO
import System.Exit (exitSuccess)
import System.IO (hFlush, isEOF, stdout)

input :: IO Input
input = do
  TextIO.putStr "> "
  hFlush stdout
  isEOF >>= \case
    True -> exitSuccess
    False ->
      Input.parse <$> TextIO.getLine >>= \case
        Just userInput -> pure userInput
        Nothing -> writeLog "Unrecognized input." >> input

-- | The debug interface currently displays activity through 'writeLog'.
output :: GameState -> IO ()
output _ = pure ()

writeLog :: Text -> IO ()
writeLog = TextIO.putStrLn
