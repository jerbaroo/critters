module Critters.State.Move (defaultMove, Move (..)) where

import Autodocodec (Autodocodec (..), HasCodec)
import Autodocodec qualified as Codec
import Critters.Codec qualified as NumberCodec
import Critters.State.MoveEffect (MoveEffect)
import Data.Aeson qualified as Aeson
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text

defaultMove :: Move
defaultMove = Move 1 "attack" 0 "defense" [] Map.empty 1 0 1 "Basic strike" 20 0 "normal"

data Move = Move
  { moveAccuracy :: Double,
    moveAttackStat :: Text,
    moveCriticalRate :: Double,
    moveDefenseStat :: Text,
    moveEffects :: [MoveEffect],
    moveHitWeights :: Map Int Int,
    moveMaxHits :: Int,
    moveMaxUses :: Int,
    moveMinHits :: Int,
    moveName :: Text,
    movePower :: Int,
    movePriority :: Int,
    moveType :: Text
  }
  deriving stock (Eq)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec Move)

instance HasCodec Move where
  codec =
    Codec.bimapCodec validate id
      $ Codec.object "Move"
      $ Move
        <$> Codec.optionalFieldWithOmittedDefaultWith "accuracy" NumberCodec.probabilityCodec 1 "Hit probability before game rules." Codec..= moveAccuracy
        <*> Codec.optionalFieldWithOmittedDefault "attackStat" "attack" "Offensive stat identifier." Codec..= moveAttackStat
        <*> Codec.optionalFieldWithOmittedDefaultWith "criticalRate" NumberCodec.probabilityCodec 0 "Critical probability before game rules." Codec..= moveCriticalRate
        <*> Codec.optionalFieldWithOmittedDefault "defenseStat" "defense" "Defensive stat identifier." Codec..= moveDefenseStat
        <*> Codec.optionalFieldWithOmittedDefault "effects" [] "Additional move effects." Codec..= moveEffects
        <*> Codec.optionalFieldWithOmittedDefault "hitWeights" Map.empty "Optional per-hit-count weights, overriding uniform selection." Codec..= moveHitWeights
        <*> Codec.optionalFieldWithOmittedDefault "maxHits" 1 "Maximum hits." Codec..= moveMaxHits
        <*> Codec.optionalFieldWithOmittedDefault "maxUses" 0 "Uses per full recovery; zero means unlimited." Codec..= moveMaxUses
        <*> Codec.optionalFieldWithOmittedDefault "minHits" 1 "Minimum hits." Codec..= moveMinHits
        <*> Codec.requiredField "name" "Display name." Codec..= moveName
        <*> Codec.optionalFieldWithOmittedDefault "power" 0 "Nonnegative base power." Codec..= movePower
        <*> Codec.optionalFieldWithOmittedDefault "priority" 0 "Higher values act first." Codec..= movePriority
        <*> Codec.optionalFieldWithOmittedDefault "type" "normal" "Game-defined type." Codec..= moveType
    where
      validate value
        | moveAccuracy value < 0 || moveAccuracy value > 1 = Left "Move accuracy must be between zero and one."
        | moveCriticalRate value < 0 || moveCriticalRate value > 1 = Left "Critical rate must be between zero and one."
        | moveMinHits value < 1 || moveMaxHits value < moveMinHits value || moveMaxHits value > 100 = Left "Hit counts must satisfy 1 <= minHits <= maxHits <= 100."
        | any (\count -> count < moveMinHits value || count > moveMaxHits value) (Map.keys (moveHitWeights value)) || any (< 1) (Map.elems (moveHitWeights value)) = Left "Hit weights must be positive and within the configured hit range."
        | moveMaxUses value < 0 || movePower value < 0 = Left "Move power and uses must not be negative."
        | any (Text.null . Text.strip) [moveName value, moveType value, moveAttackStat value, moveDefenseStat value] = Left "Move names and identifiers must not be blank."
        | otherwise = Right value
