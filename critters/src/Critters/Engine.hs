-- | Pure pipeline from Input -> Command -> Event -> State.
module Critters.Engine (applyEvent, commandFor, eventsFor, step) where

import Critters.Command (Command (..))
import Critters.Event (Event)
import Critters.Event qualified as Event
import Critters.Input (Input)
import Critters.Input qualified as Input
import Critters.Input.Direction (Direction (..))
import Critters.State.Conversation (Conversation (..))
import Critters.State.GameState (GameState (..), world)
import Critters.State.GameState qualified as GameState
import Critters.State.Location qualified as Location
import Critters.State.Npc qualified as Npc
import Critters.State.Position (Position (..))
import Critters.State.Tile (Tile (..))
import Critters.State.Warp qualified as Warp
import Critters.State.World (World (..), tileAt)
import Data.List (find)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, isNothing)

-- | Apply an established fact without checking command rules again.
applyEvent :: GameState -> Event -> GameState
applyEvent state = \case
  Event.ConversationAdvanced index -> state{conversation = fmap (\active -> active{lineIndex = index}) (conversation state)}
  Event.ConversationEnded -> state{conversation = Nothing}
  Event.ConversationStarted identifier -> state{conversation = Just (Conversation 0 identifier)}
  Event.PartyClosed -> state{partyOpen = False}
  Event.PartyOpened -> state{partyOpen = True}
  Event.PlayerMoved destination -> state{playerPosition = destination}
  Event.PlayerTurned direction -> state{playerDirection = direction}
  Event.PlayerWarped destination ->
    state{conversation = Nothing, playerMap = Location.mapId destination, playerPosition = Location.position destination}

canStand :: GameState -> Position -> Bool
canStand state destination =
  maybe False walkable (tileAt (world state) destination)
    && isNothing (GameState.npcAt state destination)

-- | The active view determines what Enter, Escape and movement mean.
commandFor :: GameState -> Input -> Maybe Command
commandFor state input
  | input == Input.Character 'q' = Just Quit
  | partyOpen state = case input of
      Input.Escape -> Just CloseMenu
      _ -> Nothing
  | otherwise = case (conversation state, input) of
      (Just _, Input.Enter) -> Just AdvanceConversation
      (Just _, Input.Escape) -> Just EndConversation
      (Just _, _) -> Nothing
      (Nothing, Input.Character 'p') -> Just OpenParty
      (Nothing, Input.Down) -> Just (Walk South)
      (Nothing, Input.Enter) -> Just Interact
      (Nothing, Input.Escape) -> Just CloseMenu
      (Nothing, Input.Left) -> Just (Walk West)
      (Nothing, Input.Right) -> Just (Walk East)
      (Nothing, Input.Up) -> Just (Walk North)
      _ -> Nothing

-- | Only valid destinations produce movement facts. Ledges are crossed in one
-- input, landing one cell beyond them in their allowed direction.
eventsFor :: GameState -> Command -> [Event]
eventsFor state command
  | partyOpen state = [Event.PartyClosed | command == CloseMenu]
  | otherwise = case command of
      AdvanceConversation -> case conversation state of
        Just active -> case Map.lookup (npcId active) (npcs state) of
          Just npc
            | Location.mapId (Npc.location npc) == playerMap state
                && lineIndex active >= 0
                && lineIndex active < length (Npc.dialogue npc) - 1 ->
                [Event.ConversationAdvanced (lineIndex active + 1)]
          _ -> [Event.ConversationEnded]
        Nothing -> []
      EndConversation -> [Event.ConversationEnded | isJust (conversation state)]
      Interact | isNothing (conversation state) ->
        case GameState.npcAt state (move (playerDirection state) (playerPosition state)) of
          Just (identifier, npc) | not (null (Npc.dialogue npc)) -> [Event.ConversationStarted identifier]
          _ -> []
      OpenParty | isNothing (conversation state) -> [Event.PartyOpened]
      Walk direction
        | isNothing (conversation state) ->
            let adjacent = move direction (playerPosition state)
                destination = case tileAt (world state) adjacent of
                  Just (Ledge allowed) | allowed == direction -> move direction adjacent
                  _ -> adjacent
                turns = [Event.PlayerTurned direction | direction /= playerDirection state]
             in turns
                  <> if isNothing (GameState.npcAt state adjacent) then movementEvents state direction destination else []
      _ -> []

move :: Direction -> Position -> Position
move direction (Position column row) = case direction of
  East -> Position (column + 1) row
  North -> Position column (row + 1)
  South -> Position column (row - 1)
  West -> Position (column - 1) row

-- | A warp is triggered only on entry, so reciprocal warps do not immediately loop.
movementEvents :: GameState -> Direction -> Position -> [Event]
movementEvents state direction destination
  | not (canStand state destination) = []
  | otherwise = case find ((== destination) . Warp.source) (warps (world state)) of
      Nothing -> [Event.PlayerMoved destination]
      Just warp
        | maybe True (== direction) (Warp.entranceDirection warp) ->
            let target = Warp.destination warp
                arrived = state{playerMap = Location.mapId target, playerPosition = Location.position target}
             in if canStand arrived (Location.position target)
                  then [Event.PlayerMoved destination, Event.PlayerWarped target]
                  else []
      _ -> []

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
  Stairs -> True
  TallGrass -> True
  Tree -> False
  Wall -> False
  Water -> False
