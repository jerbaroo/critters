module Critters.State.Party (empty, Party (..)) where

import Autodocodec (Autodocodec (..), HasCodec)
import Autodocodec qualified as Codec
import Critters.State.Critter (Critter)
import Data.Aeson qualified as Aeson

-- | The default for games that do not configure a party.
empty :: Party
empty = Party []

-- | An ordered party. The game state configures the maximum number of members.
newtype Party = Party
  { members :: [Critter]
  }
  deriving stock (Eq)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec Party)

instance HasCodec Party where
  codec =
    Codec.object "Party" $
      Party
        <$> Codec.optionalFieldWithOmittedDefault "members" [] "Critters in party order." Codec..= members
