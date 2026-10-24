module Critters.Command (Command (..)) where

import Critters.Input.Direction (Direction (..))
import Critters.Pretty (Pretty (..))

data Command
  = AdvanceConversation
  | CloseMenu
  | ConfirmSelection
  | EndConversation
  | Interact
  | Quit
  | SelectNext
  | SelectPrevious
  | Walk Direction
  deriving stock (Eq)

instance Pretty Command where
  pretty = \case
    AdvanceConversation -> "AdvanceConversation"
    CloseMenu -> "CloseMenu"
    ConfirmSelection -> "ConfirmSelection"
    EndConversation -> "EndConversation"
    Interact -> "Interact"
    Quit -> "Quit"
    SelectNext -> "SelectNext"
    SelectPrevious -> "SelectPrevious"
    Walk direction -> "Walk " <> pretty direction
