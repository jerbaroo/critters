module Critters.State.Stage (StageDefinition (..), stageDefinition, StageDefinitions (..), StageValues (..)) where

import Autodocodec (Autodocodec (..), HasCodec)
import Autodocodec qualified as Codec
import Critters.Expression (Expression)
import Critters.Expression qualified as Expression
import Critters.Pretty (pretty)
import Critters.State.StatName (StatName)
import Data.Aeson qualified as Aeson
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Witch (from)

data StageDefinition = StageDefinition
    { limit :: Int
    , multiplier :: Expression
    }
    deriving stock (Eq, Show)
    deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec StageDefinition)

-- | Missing definitions are game errors; there is no default stage rule.
stageDefinition :: StageDefinitions -> StatName -> StageDefinition
stageDefinition (StageDefinitions definitions) name =
    maybe (error ("Missing stage definition: " <> from (pretty name))) id (Map.lookup name definitions)

newtype StageDefinitions = StageDefinitions {definitions :: Map StatName StageDefinition}
    deriving stock (Eq, Show)
    deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec StageDefinitions)

newtype StageValues = StageValues {values :: Map StatName Int}
    deriving stock (Eq, Show)
    deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec StageValues)

instance HasCodec StageDefinition where
    codec =
        Codec.bimapCodec validate id $
            Codec.object "StageDefinition" $
                StageDefinition
                    <$> Codec.requiredField "limit" "Absolute limit on this temporary stage, from zero through 100." Codec..= (.limit)
                    <*> Codec.requiredField "multiplier" "Stat multiplier expression with the stage placeholder." Codec..= (.multiplier)
      where
        validate definition
            | definition.limit < 0 || definition.limit > 100 = Left "Stage limits must be between zero and 100."
            | either (const True) (any (/= "stage")) (Expression.variables definition.multiplier) = Left "Unknown placeholder in stage multiplier."
            | otherwise = Right definition

instance HasCodec StageDefinitions where
    codec = Codec.dimapCodec StageDefinitions (.definitions) Codec.codec

instance HasCodec StageValues where
    codec = Codec.dimapCodec StageValues (.values) Codec.codec
