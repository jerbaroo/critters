module Main (main) where

import Autodocodec.Yaml qualified as Codec
import Data.Text.IO qualified as Text
import State.GameState (GameState)

main :: IO ()
main = Text.putStrLn (Codec.renderPlainSchemaViaCodec @GameState)
