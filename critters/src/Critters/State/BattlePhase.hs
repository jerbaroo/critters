module Critters.State.BattlePhase (BattlePhase (..)) where

import Autodocodec (Autodocodec (..), HasCodec)
import Autodocodec qualified as Codec
import Data.Aeson qualified as Aeson
import Data.List.NonEmpty (NonEmpty (..))

data BattlePhase = ActionMenu | Finished | Introduction | ItemMenu | ItemTargetMenu | MoveMenu | ReplacementMenu | SwitchMenu
    deriving stock (Eq)
    deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec BattlePhase)

instance HasCodec BattlePhase where
    codec = Codec.stringConstCodec ((ActionMenu, "actions") :| [(Finished, "finished"), (Introduction, "introduction"), (ItemMenu, "items"), (ItemTargetMenu, "item-target"), (MoveMenu, "moves"), (ReplacementMenu, "replacement"), (SwitchMenu, "switch")])
