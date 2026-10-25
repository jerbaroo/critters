module Critters.State.BattleSide (BattleSide (..)) where

import Autodocodec (Autodocodec (..), HasCodec)
import Autodocodec qualified as Codec
import Critters.Pretty (Pretty (..))
import Data.Aeson qualified as Aeson
import Data.List.NonEmpty (NonEmpty (..))

data BattleSide = Enemy | Player
  deriving stock (Eq)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec BattleSide)

instance HasCodec BattleSide where
  codec = Codec.stringConstCodec ((Enemy, "enemy") :| [(Player, "player")])

instance Pretty BattleSide where
  pretty Enemy = "Enemy"
  pretty Player = "Player"
