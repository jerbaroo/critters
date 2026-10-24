module Critters.State.BattleDefinition (BattleDefinition (..), defaultDefinition) where

import Autodocodec (Autodocodec (..), HasCodec)
import Autodocodec qualified as Codec
import Critters.State.Critter (Critter)
import Critters.State.Effect (Effect)
import Data.Aeson qualified as Aeson
import Data.Text (Text)

data BattleDefinition = BattleDefinition
  { definitionAllowCapture :: Bool,
    definitionAllowEscape :: Bool,
    definitionCaptureEffects :: [Effect],
    definitionDefeatEffects :: [Effect],
    definitionEscapeEffects :: [Effect],
    definitionName :: Text,
    definitionOpponentPolicy :: Text,
    definitionOpponents :: [Critter],
    definitionVictoryEffects :: [Effect]
  }
  deriving stock (Eq)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec BattleDefinition)

defaultDefinition :: BattleDefinition
defaultDefinition =
  BattleDefinition
    { definitionAllowCapture = False,
      definitionAllowEscape = False,
      definitionCaptureEffects = [],
      definitionDefeatEffects = [],
      definitionEscapeEffects = [],
      definitionName = "Battle",
      definitionOpponentPolicy = "first",
      definitionOpponents = [],
      definitionVictoryEffects = []
    }

instance HasCodec BattleDefinition where
  codec =
    Codec.named "BattleDefinition"
      $ Codec.bimapCodec validate id
      $ Codec.object "BattleDefinition"
      $ BattleDefinition
        <$> Codec.optionalFieldWithOmittedDefault "allowCapture" (False) "Permit capture attempts." Codec..= definitionAllowCapture
        <*> Codec.optionalFieldWithOmittedDefault "allowEscape" (False) "Permit escape attempts." Codec..= definitionAllowEscape
        <*> Codec.optionalFieldWithOmittedDefault "captureEffects" ([]) "Effects after a capture ends the battle." Codec..= definitionCaptureEffects
        <*> Codec.optionalFieldWithOmittedDefault "defeatEffects" ([]) "Effects after defeat." Codec..= definitionDefeatEffects
        <*> Codec.optionalFieldWithOmittedDefault "escapeEffects" ([]) "Effects after escaping." Codec..= definitionEscapeEffects
        <*> Codec.optionalFieldWithOmittedDefault "name" ("Battle") "Battle title." Codec..= definitionName
        <*> Codec.optionalFieldWithOmittedDefault "opponentPolicy" ("first") "Opponent policy: first, random or strongest." Codec..= definitionOpponentPolicy
        <*> Codec.optionalFieldWithOmittedDefault "opponents" ([]) "Opposing party." Codec..= definitionOpponents
        <*> Codec.optionalFieldWithOmittedDefault "victoryEffects" ([]) "Effects after victory." Codec..= definitionVictoryEffects
    where
      validate value
        | null (definitionOpponents value) = Left "A battle needs at least one opponent."
        | definitionOpponentPolicy value `notElem` ["first", "random", "strongest"] = Left "Unknown opponent policy."
        | otherwise = Right value
