module Critters.State.StatusDefinition (defaultStatus, StatusDefinition (..)) where

import Autodocodec (Autodocodec (..), HasCodec)
import Autodocodec qualified as Codec
import Critters.Codec qualified as NumberCodec
import Data.Aeson qualified as Aeson
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)

defaultStatus :: StatusDefinition
defaultStatus =
  StatusDefinition
    { statusBlockChance = 0,
      statusCureChance = 0,
      statusDamageFraction = 0,
      statusDurationMax = 0,
      statusDurationMin = 0,
      statusImmuneTypes = [],
      statusName = "",
      statusStatMultipliers = Map.empty,
      statusTickOnApply = False
    }

data StatusDefinition = StatusDefinition
  { statusBlockChance :: Double,
    statusCureChance :: Double,
    statusDamageFraction :: Double,
    statusDurationMax :: Int,
    statusDurationMin :: Int,
    statusImmuneTypes :: [Text],
    statusName :: Text,
    statusStatMultipliers :: Map Text Double,
    statusTickOnApply :: Bool
  }
  deriving stock (Eq)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec StatusDefinition)

instance HasCodec StatusDefinition where
  codec =
    Codec.bimapCodec validate id
      $ Codec.object "StatusDefinition"
      $ StatusDefinition
        <$> Codec.optionalFieldWithOmittedDefaultWith "blockChance" NumberCodec.probabilityCodec (0) "Chance to prevent acting." Codec..= statusBlockChance
        <*> Codec.optionalFieldWithOmittedDefaultWith "cureChance" NumberCodec.probabilityCodec (0) "Chance to recover before acting." Codec..= statusCureChance
        <*> Codec.optionalFieldWithOmittedDefaultWith "damageFraction" NumberCodec.probabilityCodec (0) "Fraction of maximum HP lost per turn." Codec..= statusDamageFraction
        <*> Codec.optionalFieldWithOmittedDefault "durationMax" (0) "Maximum duration; both durations zero means persistent." Codec..= statusDurationMax
        <*> Codec.optionalFieldWithOmittedDefault "durationMin" (0) "Minimum duration." Codec..= statusDurationMin
        <*> Codec.optionalFieldWithOmittedDefault "immuneTypes" ([]) "Types immune to this status." Codec..= statusImmuneTypes
        <*> Codec.optionalFieldWithOmittedDefault "name" ("") "Display name." Codec..= statusName
        <*> Codec.optionalFieldWithOmittedDefaultWith "statMultipliers" (Codec.mapCodec NumberCodec.finiteDoubleCodec) (Map.empty) "Stat multipliers while afflicted." Codec..= statusStatMultipliers
        <*> Codec.optionalFieldWithOmittedDefault "tickOnApply" (False) "Whether residual damage applies on the turn of infliction." Codec..= statusTickOnApply
    where
      validate value
        | statusDurationMin value < 0 || statusDurationMax value < statusDurationMin value || (statusDurationMin value == 0) /= (statusDurationMax value == 0) = Left "Durations must both be zero or satisfy 1 <= min <= max."
        | any (< 0) (Map.elems (statusStatMultipliers value)) = Left "Status stat multipliers must be nonnegative."
        | otherwise = Right value
