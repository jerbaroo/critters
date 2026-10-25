module Critters.State.Condition (Condition (..)) where

import Autodocodec (Autodocodec (..), HasCodec, JSONObjectCodec)
import Autodocodec qualified as Codec
import Critters.State.FlagId (FlagId)
import Data.Aeson qualified as Aeson
import Data.HashMap.Strict qualified as HashMap

data Condition = Flag FlagId Bool
    deriving stock (Eq)
    deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec Condition)

instance HasCodec Condition where
    codec =
        Codec.object "Condition" $
            Codec.discriminatedUnionCodec "type" encode $
                HashMap.singleton "flag" ("Flag", Codec.mapToDecoder (uncurry Flag) flag)
      where
        encode (Flag identifier value) = ("flag", Codec.mapToEncoder (identifier, value) flag)
        flag :: JSONObjectCodec (FlagId, Bool)
        flag =
            (,)
                <$> Codec.requiredField "flag" "The game-defined flag identifier." Codec..= fst
                <*> Codec.requiredField "value" "The expected flag value; missing flags are false." Codec..= snd
