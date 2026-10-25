module Critters.State.BattleOutcome (BattleOutcome (..)) where

import Autodocodec (Autodocodec (..), HasCodec)
import Autodocodec qualified as Codec
import Data.Aeson qualified as Aeson
import Data.List.NonEmpty (NonEmpty (..))

data BattleOutcome = Captured | Defeat | Escaped | Victory
    deriving stock (Eq)
    deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec BattleOutcome)

instance HasCodec BattleOutcome where
    codec = Codec.stringConstCodec ((Captured, "captured") :| [(Defeat, "defeat"), (Escaped, "escaped"), (Victory, "victory")])
