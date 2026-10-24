module Critters.State.Effect (Effect (..)) where

import Autodocodec (Autodocodec (..), HasCodec, JSONObjectCodec)
import Autodocodec qualified as Codec
import Critters.State.Critter (Critter)
import Critters.State.FlagId (FlagId)
import Critters.State.ObjectId (ObjectId)
import Data.Aeson qualified as Aeson
import Data.HashMap.Strict qualified as HashMap

-- | An interaction's requested outcome, validated before events are produced.
data Effect
  = GiveCritter Critter
  | RemoveObject ObjectId
  | SetFlag FlagId Bool
  deriving stock (Eq)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec Effect)

instance HasCodec Effect where
  codec =
    Codec.object "Effect"
      $ Codec.discriminatedUnionCodec "type" encode
      $ HashMap.fromList
        [ ("give-critter", ("GiveCritter", Codec.mapToDecoder GiveCritter given)),
          ("remove-object", ("RemoveObject", Codec.mapToDecoder RemoveObject removed)),
          ("set-flag", ("SetFlag", Codec.mapToDecoder (uncurry SetFlag) set))
        ]
    where
      encode = \case
        GiveCritter critter -> ("give-critter", Codec.mapToEncoder critter given)
        RemoveObject identifier -> ("remove-object", Codec.mapToEncoder identifier removed)
        SetFlag identifier value -> ("set-flag", Codec.mapToEncoder (identifier, value) set)
      given :: JSONObjectCodec Critter
      given = Codec.requiredField "critter" "The critter to add to the party."
      removed :: JSONObjectCodec ObjectId
      removed = Codec.requiredField "objectId" "The world object to remove."
      set :: JSONObjectCodec (FlagId, Bool)
      set =
        (,)
          <$> Codec.requiredField "flag" "The game-defined flag identifier." Codec..= fst
          <*> Codec.requiredField "value" "The new flag value." Codec..= snd
