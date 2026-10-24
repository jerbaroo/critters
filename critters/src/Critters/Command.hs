module Critters.Command (Command (..)) where

import Critters.Input.Direction (Direction (..))
import Critters.Pretty (Pretty (..))

data Command
  = AdvanceDialogue
  | CloseMenu
  | ConfirmSelection
  | Quit
  | SelectNext
  | SelectPrevious
  | Walk Direction
  deriving stock (Eq)

instance Pretty Command where
  pretty = \case
    AdvanceDialogue -> "AdvanceDialogue"
    CloseMenu -> "CloseMenu"
    ConfirmSelection -> "ConfirmSelection"
    Quit -> "Quit"
    SelectNext -> "SelectNext"
    SelectPrevious -> "SelectPrevious"
    Walk direction -> "Walk " <> pretty direction
