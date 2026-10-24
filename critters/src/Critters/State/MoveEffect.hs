module Critters.State.MoveEffect (MoveEffect (..), Target (..)) where

import Autodocodec (Autodocodec (..), HasCodec, JSONObjectCodec)
import Autodocodec qualified as Codec
import Critters.Codec qualified as NumberCodec
import Critters.Formula (Formula)
import Data.Aeson qualified as Aeson
import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)

data MoveEffect
  = Chance Double MoveEffect
  | ChangeHealth Target Formula
  | ChangeStage Target Text Int
  | ClearStatus Target
  | InflictStatus Target Text
  deriving stock (Eq)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec MoveEffect)

data Target = Opponent | Self
  deriving stock (Eq)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec Target)

instance HasCodec MoveEffect where
  codec =
    Codec.named "MoveEffect"
      $ Codec.object "MoveEffect"
      $ Codec.discriminatedUnionCodec "type" encode
      $ HashMap.fromList
        [ ("chance", ("Chance", Codec.mapToDecoder (uncurry Chance) chance)),
          ("change-health", ("ChangeHealth", Codec.mapToDecoder (uncurry ChangeHealth) health)),
          ("change-stage", ("ChangeStage", Codec.mapToDecoder (\(destination, stat, amount) -> ChangeStage destination stat amount) stage)),
          ("clear-status", ("ClearStatus", Codec.mapToDecoder ClearStatus target)),
          ("inflict-status", ("InflictStatus", Codec.mapToDecoder (uncurry InflictStatus) status))
        ]
    where
      chance :: JSONObjectCodec (Double, MoveEffect)
      chance = (,) <$> Codec.requiredFieldWith "probability" NumberCodec.probabilityCodec "Probability from zero to one." Codec..= fst <*> Codec.requiredField "effect" "The conditional effect." Codec..= snd
      encode = \case
        Chance value effect -> ("chance", Codec.mapToEncoder (value, effect) chance)
        ChangeHealth destination amount -> ("change-health", Codec.mapToEncoder (destination, amount) health)
        ChangeStage destination stat amount -> ("change-stage", Codec.mapToEncoder (destination, stat, amount) stage)
        ClearStatus destination -> ("clear-status", Codec.mapToEncoder destination target)
        InflictStatus destination identifier -> ("inflict-status", Codec.mapToEncoder (destination, identifier) status)
      health :: JSONObjectCodec (Target, Formula)
      health = (,) <$> target Codec..= fst <*> Codec.requiredField "amount" "Signed HP change; supports damage, hp and maxHP variables." Codec..= snd
      stage :: JSONObjectCodec (Target, Text, Int)
      stage = (,,) <$> target Codec..= (\(value, _, _) -> value) <*> Codec.requiredField "stat" "The stat identifier." Codec..= (\(_, value, _) -> value) <*> Codec.requiredField "amount" "The stage change." Codec..= (\(_, _, value) -> value)
      status :: JSONObjectCodec (Target, Text)
      status = (,) <$> target Codec..= fst <*> Codec.requiredField "status" "The status identifier." Codec..= snd
      target :: JSONObjectCodec Target
      target = Codec.optionalFieldWithOmittedDefault "target" Opponent "The affected side."

instance HasCodec Target where
  codec = Codec.stringConstCodec ((Opponent, "opponent") :| [(Self, "self")])
