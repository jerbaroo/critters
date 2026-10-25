module Critters.State.StatusDefinition (defaultStatus, StatusDefinition (..)) where

import Autodocodec (Autodocodec (..), HasCodec)
import Autodocodec qualified as Codec
import Critters.Codec qualified as NumberCodec
import Critters.State.StatName (StatName)
import Data.Aeson qualified as Aeson
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)

defaultStatus :: StatusDefinition
defaultStatus =
    StatusDefinition
        { blockChance = 0
        , cureChance = 0
        , damageFraction = 0
        , durationMax = 0
        , durationMin = 0
        , immuneTypes = []
        , name = ""
        , statMultipliers = Map.empty
        , tickOnApply = False
        }

data StatusDefinition = StatusDefinition
    { blockChance :: Double
    , cureChance :: Double
    , damageFraction :: Double
    , durationMax :: Int
    , durationMin :: Int
    , immuneTypes :: [Text]
    , name :: Text
    , statMultipliers :: Map StatName Double
    , tickOnApply :: Bool
    }
    deriving stock (Eq)
    deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec StatusDefinition)

instance HasCodec StatusDefinition where
    codec =
        Codec.bimapCodec validate id $
            Codec.object "StatusDefinition" $
                StatusDefinition
                    <$> Codec.optionalFieldWithOmittedDefaultWith "blockChance" NumberCodec.probabilityCodec (0) "Chance to prevent acting." Codec..= (.blockChance)
                    <*> Codec.optionalFieldWithOmittedDefaultWith "cureChance" NumberCodec.probabilityCodec (0) "Chance to recover before acting." Codec..= (.cureChance)
                    <*> Codec.optionalFieldWithOmittedDefaultWith "damageFraction" NumberCodec.probabilityCodec (0) "Fraction of maximum HP lost per turn." Codec..= (.damageFraction)
                    <*> Codec.optionalFieldWithOmittedDefault "durationMax" (0) "Maximum duration; both durations zero means persistent." Codec..= (.durationMax)
                    <*> Codec.optionalFieldWithOmittedDefault "durationMin" (0) "Minimum duration." Codec..= (.durationMin)
                    <*> Codec.optionalFieldWithOmittedDefault "immuneTypes" ([]) "Types immune to this status." Codec..= (.immuneTypes)
                    <*> Codec.optionalFieldWithOmittedDefault "name" ("") "Display name." Codec..= (.name)
                    <*> Codec.optionalFieldWithOmittedDefaultWith "statMultipliers" (Codec.mapCodec NumberCodec.finiteDoubleCodec) (Map.empty) "Stat multipliers while afflicted." Codec..= (.statMultipliers)
                    <*> Codec.optionalFieldWithOmittedDefault "tickOnApply" (False) "Whether residual damage applies on the turn of infliction." Codec..= (.tickOnApply)
      where
        validate value
            | value.durationMin < 0 || value.durationMax < value.durationMin || (value.durationMin == 0) /= (value.durationMax == 0) = Left "Durations must both be zero or satisfy 1 <= min <= max."
            | any (< 0) (Map.elems value.statMultipliers) = Left "Status stat multipliers must be nonnegative."
            | otherwise = Right value
