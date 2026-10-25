module Critters.State.Move (defaultMove, Move (..)) where

import Autodocodec (Autodocodec (..), HasCodec)
import Autodocodec qualified as Codec
import Critters.Codec qualified as NumberCodec
import Critters.Pretty (pretty)
import Critters.State.MoveEffect (MoveEffect)
import Critters.State.StatName (StatName)
import Data.Aeson qualified as Aeson
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text

defaultMove :: Move
defaultMove = Move 1 "normal" "attack" 0 "defense" [] Map.empty 1 0 1 "{messages.battleFallbackMoveName}" 20 0

data Move = Move
    { accuracy :: Double
    , affinity :: Text
    , attackStat :: StatName
    , criticalRate :: Double
    , defenseStat :: StatName
    , effects :: [MoveEffect]
    , hitWeights :: Map Int Int
    , maxHits :: Int
    , maxUses :: Int
    , minHits :: Int
    , name :: Text
    , power :: Int
    , priority :: Int
    }
    deriving stock (Eq)
    deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec Move)

instance HasCodec Move where
    codec =
        Codec.bimapCodec validate id $
            Codec.object "Move" $
                Move
                    <$> Codec.optionalFieldWithOmittedDefaultWith "accuracy" NumberCodec.probabilityCodec 1 "Hit probability before game rules." Codec..= (.accuracy)
                    <*> Codec.optionalFieldWithOmittedDefault "type" "normal" "Game-defined type." Codec..= (.affinity)
                    <*> Codec.optionalFieldWithOmittedDefault "attackStat" "attack" "Offensive stat identifier." Codec..= (.attackStat)
                    <*> Codec.optionalFieldWithOmittedDefaultWith "criticalRate" NumberCodec.probabilityCodec 0 "Critical probability before game rules." Codec..= (.criticalRate)
                    <*> Codec.optionalFieldWithOmittedDefault "defenseStat" "defense" "Defensive stat identifier." Codec..= (.defenseStat)
                    <*> Codec.optionalFieldWithOmittedDefault "effects" [] "Additional move effects." Codec..= (.effects)
                    <*> Codec.optionalFieldWithOmittedDefault "hitWeights" Map.empty "Optional per-hit-count weights, overriding uniform selection." Codec..= (.hitWeights)
                    <*> Codec.optionalFieldWithOmittedDefault "maxHits" 1 "Maximum hits." Codec..= (.maxHits)
                    <*> Codec.optionalFieldWithOmittedDefault "maxUses" 0 "Uses per full recovery; zero means unlimited." Codec..= (.maxUses)
                    <*> Codec.optionalFieldWithOmittedDefault "minHits" 1 "Minimum hits." Codec..= (.minHits)
                    <*> Codec.requiredField "name" "Display name." Codec..= (.name)
                    <*> Codec.optionalFieldWithOmittedDefault "power" 0 "Nonnegative base power." Codec..= (.power)
                    <*> Codec.optionalFieldWithOmittedDefault "priority" 0 "Higher values act first." Codec..= (.priority)
      where
        validate value
            | value.accuracy < 0 || value.accuracy > 1 = Left "Move accuracy must be between zero and one."
            | value.criticalRate < 0 || value.criticalRate > 1 = Left "Critical rate must be between zero and one."
            | value.minHits < 1 || value.maxHits < value.minHits || value.maxHits > 100 = Left "Hit counts must satisfy 1 <= minHits <= maxHits <= 100."
            | any (\count -> count < value.minHits || count > value.maxHits) (Map.keys value.hitWeights) || any (< 1) (Map.elems value.hitWeights) = Left "Hit weights must be positive and within the configured hit range."
            | value.maxUses < 0 || value.power < 0 = Left "Move power and uses must not be negative."
            | any (Text.null . Text.strip) [value.name, value.affinity, pretty value.attackStat, pretty value.defenseStat] = Left "Move names and identifiers must not be blank."
            | otherwise = Right value
