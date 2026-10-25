module Critters.Interface.TUI.Keyboard (discardInput, input, withTerminal) where

import Control.Exception (bracket, bracket_, finally)
import Critters.Input (Input)
import Critters.Input qualified as Input
import System.Exit (exitSuccess)
import System.IO (BufferMode (NoBuffering), hFlush, hGetBuffering, hGetEcho, hIsTerminalDevice, hLookAhead, hSetBuffering, hSetEcho, hWaitForInput, isEOF, stdin, stdout)
import System.IO.Error (catchIOError, isEOFError)

-- | Playback keys must not become commands when the battle menu returns.
discardInput :: IO ()
discardInput = do
  terminal <- hIsTerminalDevice stdin
  whenReady terminal
  where
    whenReady False = pure ()
    whenReady True = do
      available <- catchIOError (hWaitForInput stdin 0) (\err -> if isEOFError err then pure False else ioError err)
      if available
        then
          input >>= \case
            Just (Input.Character 'q') -> exitSuccess
            _ -> whenReady True
        else pure ()

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

withHiddenCursor :: IO a -> IO a
withHiddenCursor action =
  hIsTerminalDevice stdout >>= \case
    False -> action
    True ->
      bracket_
        (putStr "\ESC[?25l" >> hFlush stdout)
        (putStr "\ESC[?25h" >> hFlush stdout)
        action

-- | Hide the cursor and read individual keys without echo, restoring on exit.
withTerminal :: IO a -> IO a
withTerminal action =
  withHiddenCursor $
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
