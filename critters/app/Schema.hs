module Main (main) where

import Critters.Schema (gameSchema)
import Data.Aeson.Encode.Pretty qualified as Aeson
import Data.ByteString.Lazy.Char8 qualified as ByteString

main :: IO ()
main = ByteString.putStrLn (Aeson.encodePretty' Aeson.defConfig{Aeson.confCompare = compare, Aeson.confIndent = Aeson.Spaces 2} gameSchema)
