module Critters.Battle.Action (Action (..)) where

import Critters.Pretty (Pretty (..))
import Critters.State.MoveId (MoveId)
import Data.Text (Text)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy.Builder.Int (decimal)
import Witch (from)

data Action
    = EscapeBattle
    | SwitchCritter Int
    | UseItem Text Int
    | UseMove MoveId
    deriving stock (Eq)

instance Pretty Action where
    pretty = \case
        EscapeBattle -> "EscapeBattle"
        SwitchCritter index -> "SwitchCritter " <> from (toLazyText (decimal index))
        UseItem identifier index -> "UseItem " <> identifier <> " " <> from (toLazyText (decimal index))
        UseMove identifier -> "UseMove " <> pretty identifier
