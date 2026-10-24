module Main (main) where

import Autodocodec.Yaml qualified as Codec
import Critters.State.GameState (GameState)
import Data.Text.IO qualified as Text

main :: IO ()
main = Text.putStrLn (Codec.renderPlainSchemaViaCodec @GameState)
