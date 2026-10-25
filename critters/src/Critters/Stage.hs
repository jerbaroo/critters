module Critters.Stage (changeStage, StageChange (..), stageFactor, StageOutcome (..), stageOutcome) where

import Critters.Expression qualified as Expression
import Critters.Number (number)
import Critters.State.Stage (StageDefinition (..), StageDefinitions, StageValues (..), stageDefinition)
import Critters.State.StatName (StatName)
import Data.Map.Strict qualified as Map
import Witch (from)

-- | Apply a bounded change to one stat, leaving every other stage untouched.
changeStage :: StageDefinitions -> StageValues -> StatName -> Int -> (StageValues, StageChange)
changeStage definitions (StageValues values) name amount =
    (StageValues (Map.insert name next values), StageChange next current amount name)
  where
    clamp = max (-limit) . min limit
    current = Map.findWithDefault 0 name values
    limit = (stageDefinition definitions name).limit
    next = clamp (clamp current + clamp amount)

data StageChange = StageChange
    { after :: Int
    , before :: Int
    , requested :: Int
    , stat :: StatName
    }
    deriving stock (Eq, Show)

-- | A defined stage with no temporary value starts at zero.
stageFactor :: StageDefinitions -> StageValues -> StatName -> Double
stageFactor definitions (StageValues values) name =
    max 0 (either (error . from) id (Expression.evaluateNumber (Map.singleton "stage" (number stage)) definition.multiplier))
  where
    definition = stageDefinition definitions name
    stage = Map.findWithDefault 0 name values

data StageOutcome
    = Fell Int
    | LowerLimit
    | Rose Int
    | Unchanged
    | UpperLimit
    deriving stock (Eq, Show)

stageOutcome :: StageChange -> StageOutcome
stageOutcome change
    | change.after < change.before = Fell (change.before - change.after)
    | change.after > change.before = Rose (change.after - change.before)
    | change.requested < 0 = LowerLimit
    | change.requested > 0 = UpperLimit
    | otherwise = Unchanged
