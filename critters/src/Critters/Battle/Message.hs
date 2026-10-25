module Critters.Battle.Message (stageMessage) where

import Critters.Battle.Common (Side (..))
import Critters.Expression (Value (..))
import Critters.Message qualified as Message
import Critters.Pretty (pretty)
import Critters.Stage (StageChange (..), StageOutcome (..), stageOutcome)
import Critters.Stage qualified as Stage
import Critters.State.CombatConfig (CombatConfig (..))
import Critters.State.Message (Argument (..), Message)
import Critters.State.Stage (StageValues (..))
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Witch (from)

stageMessage :: CombatConfig -> Side -> Text -> StageChange -> Message
stageMessage config side species change =
    Message.named
        key
        [ ("after", Message.numeric change.after)
        , ("afterMultiplier", Literal (NumberValue (factor change.after)))
        , ("afterStage", Message.text (signed change.after))
        , ("amount", Message.numeric (abs (change.after - change.before)))
        , ("before", Message.numeric change.before)
        , ("beforeMultiplier", Literal (NumberValue (factor change.before)))
        , ("beforeStage", Message.text (signed change.before))
        , ("requested", Message.numeric change.requested)
        , ("species", Message.reference ("{combat.species." <> species <> ".name}"))
        , ("stat", Message.reference ("{combat.statNames." <> pretty change.stat <> "}"))
        , ("subject", Message.reference (case side of Enemy -> "{messages.stageEnemySubject}"; Player -> "{messages.stagePlayerSubject}"))
        ]
  where
    factor stage = Stage.stageFactor config.stages (StageValues (Map.singleton change.stat stage)) change.stat
    key = case stageOutcome change of
        Fell _ -> "stageFell"
        LowerLimit -> "stageMinimum"
        Rose _ -> "stageRose"
        Unchanged -> "stageUnchanged"
        UpperLimit -> "stageMaximum"
    signed stage = (if stage > 0 then "+" else "") <> from (show stage)
