module Critters.Command (Command (..), fromInput) where

import Critters.Input (Input)
import Critters.Input qualified as Input
import Critters.Input.Direction (Direction (..))
import Critters.Pretty (Pretty (..))
import Critters.State.GameState (GameState)

data Command
  = AdvanceDialogue
  | CloseMenu
  | ConfirmSelection
  | Quit
  | SelectNext
  | SelectPrevious
  | Walk Direction
  deriving stock (Eq)

-- | Bindings are fixed for now; the state will determine the interaction context.
fromInput :: GameState -> Input -> Maybe Command
fromInput _ = \case
  Input.Character 'q' -> Just Quit
  Input.Down -> Just (Walk South)
  Input.Enter -> Just ConfirmSelection
  Input.Escape -> Just CloseMenu
  Input.Left -> Just (Walk West)
  Input.Right -> Just (Walk East)
  Input.Up -> Just (Walk North)
  _ -> Nothing

instance Pretty Command where
  pretty = \case
    AdvanceDialogue -> "AdvanceDialogue"
    CloseMenu -> "CloseMenu"
    ConfirmSelection -> "ConfirmSelection"
    Quit -> "Quit"
    SelectNext -> "SelectNext"
    SelectPrevious -> "SelectPrevious"
    Walk direction -> "Walk " <> pretty direction
