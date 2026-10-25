module Critters.State.Affliction (Affliction (..)) where

import Autodocodec (Autodocodec (..), HasCodec)
import Autodocodec qualified as Codec
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Data.Text qualified as Text

data Affliction = Affliction
    { remaining :: Maybe Int
    , status :: Text
    }
    deriving stock (Eq)
    deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec Affliction)

instance HasCodec Affliction where
    codec =
        Codec.bimapCodec validate id $
            Codec.object "Affliction" $
                Affliction
                    <$> Codec.optionalFieldOrNull "remaining" "Remaining turns; absent means until cured." Codec..= (.remaining)
                    <*> Codec.requiredField "status" "Game-defined status identifier." Codec..= (.status)
      where
        validate value
            | maybe False (< 1) value.remaining = Left "Status duration must be positive."
            | Text.null (Text.strip value.status) = Left "Status identifiers must not be blank."
            | otherwise = Right value
