module Critters.Save (apply, load, save) where

import Critters.Engine qualified as Engine
import Critters.Event (Event)
import Critters.State.GameState (GameState)
import Data.Aeson qualified as Aeson
import Data.Bifunctor (first)
import Data.ByteString.Char8 qualified as ByteString
import Data.ByteString.Lazy.Char8 qualified as LazyByteString
import Data.Text (Text)
import System.IO.Error (ioeGetErrorString, tryIOError)
import Witch (from)

-- | Replay established facts in order against the given starting state.
apply :: GameState -> [Event] -> GameState
apply = foldl' Engine.applyEvent

decode :: ByteString.ByteString -> Either Text [Event]
decode contents = case Aeson.eitherDecodeStrict contents of
    Right events -> Right events
    Left _ -> traverse (first from . Aeson.eitherDecodeStrict) (ByteString.lines contents)

-- | Read one JSON event per line. Legacy JSON-array histories are also accepted.
load :: FilePath -> IO (Either Text [Event])
load path =
    tryIOError (ByteString.readFile path) >>= \case
        Left err -> pure $ Left $ from (ioeGetErrorString err)
        Right contents -> pure $ decode contents

-- | Replace the file with one JSON event per line. An empty history writes an empty file.
save :: FilePath -> [Event] -> IO (Either Text ())
save path events =
    first (from . ioeGetErrorString)
        <$> tryIOError (LazyByteString.writeFile path (LazyByteString.unlines (map Aeson.encode events)))
