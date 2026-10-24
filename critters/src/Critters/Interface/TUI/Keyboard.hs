module Critters.Interface.TUI.Keyboard (input, withTerminal) where

import Control.Exception (bracket, finally)
import Critters.Input (Input)
import Critters.Input qualified as Input
import System.Exit (exitSuccess)
import System.IO (BufferMode (NoBuffering), hGetBuffering, hGetEcho, hIsTerminalDevice, hLookAhead, hSetBuffering, hSetEcho, hWaitForInput, isEOF, stdin)
import System.IO.Error (catchIOError, isEOFError)

input :: IO (Maybe Input)
input =
  isEOF >>= \case
    True -> exitSuccess
    False ->
      getChar >>= \case
        '\ETX' -> exitSuccess
        '\EOT' -> exitSuccess
        '\ESC' -> readEscape
        '\n' -> pure (Just Input.Enter)
        '\r' -> pure (Just Input.Enter)
        character -> pure (Just (Input.Character character))

readEscape :: IO (Maybe Input)
readEscape =
  ready >>= \case
    False -> pure (Just Input.Escape)
    True ->
      hLookAhead stdin >>= \case
        '[' -> getChar >> readSequence
        'O' -> getChar >> readSequence
        _ -> pure (Just Input.Escape)

readSequence :: IO (Maybe Input)
readSequence = go (0 :: Int)
  where
    go count
      | count >= 16 = pure Nothing
      | otherwise =
          ready >>= \case
            False -> pure Nothing
            True ->
              getChar >>= \case
                'A' -> pure (Just Input.Up)
                'B' -> pure (Just Input.Down)
                'C' -> pure (Just Input.Right)
                'D' -> pure (Just Input.Left)
                character
                  | character >= '@' && character <= '~' -> pure Nothing
                  | otherwise -> go (count + 1)

-- | Allow a short delay to distinguish Escape from an escape sequence.
ready :: IO Bool
ready =
  catchIOError (hWaitForInput stdin 50) $ \err ->
    if isEOFError err then pure False else ioError err

-- | Read individual keys without echo, restoring the original modes on exit.
withTerminal :: IO a -> IO a
withTerminal action =
  hIsTerminalDevice stdin >>= \case
    False -> action
    True ->
      bracket
        ((,) <$> hGetBuffering stdin <*> hGetEcho stdin)
        (\(buffering, echo) -> hSetBuffering stdin buffering `finally` hSetEcho stdin echo)
        $ \_ -> do
          hSetBuffering stdin NoBuffering
          hSetEcho stdin False
          action
