module Critters.State.BattleDefinition (BattleDefinition (..), defaultDefinition) where

import Autodocodec (Autodocodec (..), HasCodec)
import Autodocodec qualified as Codec
import Critters.State.Critter (Critter)
import Critters.State.Effect (Effect)
import Data.Aeson qualified as Aeson
import Data.Text (Text)

data BattleDefinition = BattleDefinition
    { allowCapture :: Bool
    , allowEscape :: Bool
    , captureEffects :: [Effect]
    , defeatEffects :: [Effect]
    , escapeEffects :: [Effect]
    , introduction :: [Text]
    , name :: Text
    , opponentPolicy :: Text
    , opponents :: [Critter]
    , victoryEffects :: [Effect]
    }
    deriving stock (Eq)
    deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec BattleDefinition)

defaultDefinition :: BattleDefinition
defaultDefinition =
    BattleDefinition
        { allowCapture = False
        , allowEscape = False
        , captureEffects = []
        , defeatEffects = []
        , escapeEffects = []
        , introduction = []
        , name = "Battle"
        , opponentPolicy = "first"
        , opponents = []
        , victoryEffects = []
        }

instance HasCodec BattleDefinition where
    codec =
        Codec.named "BattleDefinition" $
            Codec.bimapCodec validate id $
                Codec.object "BattleDefinition" $
                    BattleDefinition
                        <$> Codec.optionalFieldWithOmittedDefault "allowCapture" (False) "Permit capture attempts." Codec..= (.allowCapture)
                        <*> Codec.optionalFieldWithOmittedDefault "allowEscape" (False) "Permit escape attempts." Codec..= (.allowEscape)
                        <*> Codec.optionalFieldWithOmittedDefault "captureEffects" ([]) "Effects after a capture ends the battle." Codec..= (.captureEffects)
                        <*> Codec.optionalFieldWithOmittedDefault "defeatEffects" ([]) "Effects after defeat." Codec..= (.defeatEffects)
                        <*> Codec.optionalFieldWithOmittedDefault "escapeEffects" ([]) "Effects after escaping." Codec..= (.escapeEffects)
                        <*> Codec.optionalFieldWithOmittedDefault "introduction" [] "Dialogue before combatants are sent out." Codec..= (.introduction)
                        <*> Codec.optionalFieldWithOmittedDefault "name" ("Battle") "Battle title." Codec..= (.name)
                        <*> Codec.optionalFieldWithOmittedDefault "opponentPolicy" ("first") "Opponent policy: first, random or strongest." Codec..= (.opponentPolicy)
                        <*> Codec.optionalFieldWithOmittedDefault "opponents" ([]) "Opposing party." Codec..= (.opponents)
                        <*> Codec.optionalFieldWithOmittedDefault "victoryEffects" ([]) "Effects after victory." Codec..= (.victoryEffects)
      where
        validate value
            | null value.opponents = Left "A battle needs at least one opponent."
            | value.opponentPolicy `notElem` ["first", "random", "strongest"] = Left "Unknown opponent policy."
            | otherwise = Right value
