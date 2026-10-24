module Critters.State.Affliction (Affliction (..)) where

import Autodocodec qualified as Codec
import Autodocodec (Autodocodec (..), HasCodec)
import Data.Aeson qualified as Aeson
import Data.Text qualified as Text
import Data.Text (Text)

data Affliction = Affliction
  { afflictionRemaining :: Maybe Int,
    afflictionStatus :: Text
  }
  deriving stock (Eq)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec Affliction)

instance HasCodec Affliction where
  codec = Codec.bimapCodec validate id $ Codec.object "Affliction" $ Affliction
    <$> Codec.optionalFieldOrNull "remaining" "Remaining turns; absent means until cured." Codec..= afflictionRemaining
    <*> Codec.requiredField "status" "Game-defined status identifier." Codec..= afflictionStatus
    where
      validate value
        | maybe False (< 1) (afflictionRemaining value) = Left "Status duration must be positive."
        | Text.null (Text.strip (afflictionStatus value)) = Left "Status identifiers must not be blank."
        | otherwise = Right value
