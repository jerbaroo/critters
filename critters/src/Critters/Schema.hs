module Critters.Schema (gameSchema) where

import Autodocodec.Schema (JSONSchema (..), ObjectSchema (..))
import Autodocodec.Schema qualified as Schema
import Control.Monad.Trans.State.Strict (State)
import Control.Monad.Trans.State.Strict qualified as State
import Critters.State.GameState (GameState)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)

gameSchema :: JSONSchema
gameSchema = hoistDefinitions (Schema.jsonSchemaViaCodec @GameState)

-- | Autodocodec uses absolute references, so definitions must share the root.
hoistDefinitions :: JSONSchema -> JSONSchema
hoistDefinitions root =
    let (schema, definitions) = State.runState (value root) Map.empty
     in if Map.null definitions then schema else WithDefSchema definitions schema
  where
    object :: ObjectSchema -> State (Map Text JSONSchema) ObjectSchema
    object = \case
        ObjectAllOfSchema schemas -> ObjectAllOfSchema <$> traverse object schemas
        ObjectAnyOfSchema schemas -> ObjectAnyOfSchema <$> traverse object schemas
        ObjectAnySchema -> pure ObjectAnySchema
        ObjectKeySchema key requirement schema comment -> (\next -> ObjectKeySchema key requirement next comment) <$> value schema
        ObjectOneOfSchema schemas -> ObjectOneOfSchema <$> traverse object schemas
    value :: JSONSchema -> State (Map Text JSONSchema) JSONSchema
    value = \case
        AnyOfSchema schemas -> AnyOfSchema <$> traverse value schemas
        ArraySchema schema -> ArraySchema <$> value schema
        CommentSchema comment schema -> CommentSchema comment <$> value schema
        MapSchema schema -> MapSchema <$> value schema
        ObjectSchema schema -> ObjectSchema <$> object schema
        OneOfSchema schemas -> OneOfSchema <$> traverse value schemas
        WithDefSchema definitions schema -> do
            lifted <- traverse value definitions
            State.modify' (Map.union lifted)
            value schema
        schema -> pure schema
