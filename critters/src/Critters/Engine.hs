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
import Critters.State.Tile (Tile (..))
import Critters.State.World (tileAt)

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

-- | Only valid destinations produce movement facts. Ledges are crossed in one
-- input, landing one cell beyond them in their allowed direction.
eventsFor :: GameState -> Command -> [Event]
eventsFor state = \case
  Walk direction ->
    let adjacent = move direction (playerPosition state)
        destination = case tileAt (world state) adjacent of
          Just (Ledge allowed) | allowed == direction -> move direction adjacent
          _ -> adjacent
     in if maybe False walkable (tileAt (world state) destination)
          then [Event.PlayerMoved destination]
          else []
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

walkable :: Tile -> Bool
walkable = \case
  Door -> True
  Fence -> False
  Flowers -> True
  Grass -> True
  Ground -> True
  Arena -> False
  House -> False
  Laboratory -> False
  Ledge _ -> False
  Shop -> False
  Path -> True
  Clinic -> False
  Rock -> False
  Sand -> True
  Sign -> False
  TallGrass -> True
  Tree -> False
  Wall -> False
  Water -> False
