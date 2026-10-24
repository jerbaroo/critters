-- | Pure pipeline from Input -> Command -> Event -> State.
module Critters.Engine (applyEvent, commandFor, eventsFor, step) where

import Critters.Command (Command (..))
import Critters.Event (Event)
import Critters.Event qualified as Event
import Critters.Input (Input)
import Critters.Input qualified as Input
import Critters.Input.Direction (Direction (..))
import Critters.State.GameState (GameState (..))
import Critters.State.Position (Position (..))

-- | Apply an established fact without checking command rules again.
applyEvent :: GameState -> Event -> GameState
applyEvent state (Event.PlayerMoved destination) = state{playerPosition = destination}

-- | Bindings are fixed for now; the state will determine the interaction context.
commandFor :: GameState -> Input -> Maybe Command
commandFor _ = \case
  Input.Character 'q' -> Just Quit
  Input.Down -> Just (Walk South)
  Input.Enter -> Just ConfirmSelection
  Input.Escape -> Just CloseMenu
  Input.Left -> Just (Walk West)
  Input.Right -> Just (Walk East)
  Input.Up -> Just (Walk North)
  _ -> Nothing

-- | Walking produces a movement fact; other commands have no state changes yet.
eventsFor :: GameState -> Command -> [Event]
eventsFor state = \case
  Walk direction -> [Event.PlayerMoved (move direction (playerPosition state))]
  _ -> []

move :: Direction -> Position -> Position
move direction (Position column row) = case direction of
  East -> Position (column + 1) row
  North -> Position column (row + 1)
  South -> Position column (row - 1)
  West -> Position (column - 1) row

-- | Interpret an input and return the resulting state and recorded facts.
step :: GameState -> Input -> (GameState, [Event])
step state input = (foldl' applyEvent state events, events)
  where
    events = maybe [] (eventsFor state) (commandFor state input)
