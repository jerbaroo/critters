module Critters.Battle.Message (renderMessage, renderStageChange) where

import Critters.Battle.Common (Side (..))
import Critters.Expression (Value (..))
import Critters.Expression qualified as Expression
import Critters.Number (number)
import Critters.Pretty (pretty)
import Critters.Stage (StageChange (..), StageOutcome (..), stageOutcome)
import Critters.Stage qualified as Stage
import Critters.State.CombatConfig (CombatConfig (..))
import Critters.State.Species qualified as Species
import Critters.State.Stage (StageValues (..))
import Data.Char (toUpper)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Witch (from)

renderMessage :: CombatConfig -> Text -> Map Text Value -> Text
renderMessage config key environment =
    case Map.lookup key config.messages of
        Nothing -> error ("Missing battle message: " <> from key)
        Just template -> either (error . from . (("Battle message " <> key <> ": ") <>)) id (Expression.interpolate environment template)

renderStageChange :: CombatConfig -> Side -> Text -> StageChange -> Text
renderStageChange config side identifier change = renderMessage config key environment
  where
    environment =
        Map.fromList
            [ ("after", NumberValue (number change.after))
            , ("afterMultiplier", NumberValue (factor change.after))
            , ("afterStage", TextValue (signed change.after))
            , ("amount", NumberValue (number (abs (change.after - change.before))))
            , ("before", NumberValue (number change.before))
            , ("beforeMultiplier", NumberValue (factor change.before))
            , ("beforeStage", TextValue (signed change.before))
            , ("requested", NumberValue (number change.requested))
            , ("species", TextValue species)
            , ("stagePlural", TextValue (renderMessage config "stagePlural" Map.empty))
            , ("stageSingular", TextValue (renderMessage config "stageSingular" Map.empty))
            , ("stat", TextValue stat)
            , ("subject", TextValue (renderMessage config subject (Map.singleton "species" (TextValue species))))
            ]
    factor stage = Stage.stageFactor config.stages (StageValues (Map.singleton change.stat stage)) change.stat
    key = case stageOutcome change of
        Fell _ -> "stageFell"
        LowerLimit -> "stageMinimum"
        Rose _ -> "stageRose"
        Unchanged -> "stageUnchanged"
        UpperLimit -> "stageMaximum"
    signed stage = (if stage > 0 then "+" else "") <> from (show stage)
    species = case Map.lookup identifier config.species of
        Just definition | not (Text.null definition.name) -> definition.name
        _ -> case Text.uncons identifier of
            Just (first, rest) -> Text.cons (toUpper first) rest
            Nothing -> identifier
    stat = maybe (error ("Missing stat display name: " <> from (pretty change.stat))) id (Map.lookup change.stat config.statNames)
    subject = case side of
        Enemy -> "stageEnemySubject"
        Player -> "stagePlayerSubject"
