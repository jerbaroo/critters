module Critters.Command (Command (..)) where

import Critters.Input.Direction (Direction)

data Command
  = AdvanceDialogue
  | CloseMenu
  | ConfirmSelection
  | Quit
  | SelectNext
  | SelectPrevious
  | Walk Direction
  deriving stock (Eq)
