-- | Pure pipeline from Input -> Command -> Event -> State.
module Critters.Engine (applyEvent, chooseInteraction, commandFor, conditionHolds, eventsFor, eventsForEffects, step) where

import Control.Monad (foldM)
import Critters.Battle.Common qualified as BattleCommon
import Critters.Battle.Engine qualified as BattleEngine
import Critters.State.CombatConfig qualified as Config
import Critters.Command (Command (..))
import Critters.Event (Event)
import Critters.Event qualified as Event
import Critters.Input (Input)
import Critters.Input qualified as Input
import Critters.Input.Direction (Direction (..))
import Critters.State.ActiveInteraction (ActiveInteraction (..))
import Critters.State.Battle (Battle)
import Critters.State.Battle qualified as Battle
import Critters.State.BattlePhase (BattlePhase (Finished))
import Critters.State.BattleUpdate (BattleUpdate (..))
import Critters.State.Choice qualified as Choice
import Critters.State.Condition (Condition (..))
import Critters.State.Conversation (Conversation (..))
import Critters.State.Effect (Effect)
import Critters.State.Effect qualified as Effect
import Critters.State.GameState (GameState (..), world)
import Critters.State.GameState qualified as GameState
import Critters.State.Interaction (Interaction (..))
import Critters.State.Location qualified as Location
import Critters.State.Npc qualified as Npc
import Critters.State.Object qualified as Object
import Critters.State.Party (Party (..))
import Critters.State.Position (Position (..))
import Critters.State.Tile (Tile (..))
import Critters.State.Warp qualified as Warp
import Critters.State.World (World (..), tileAt)
import Data.List (find)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Text (Text)

advanceInteraction :: GameState -> ActiveInteraction -> [Event]
advanceInteraction state active
  | isJust (activeSelectedChoice active) = []
  | index < 0 || index >= length dialogue = [Event.InteractionEnded]
  | index < length dialogue - 1 =
      [Event.InteractionAdvanced (index + 1)]
        <> [Event.InteractionChoiceOpened | index + 1 == length dialogue - 1 && not (null choices)]
  | not (null choices) = [Event.InteractionChoiceOpened]
  | otherwise = completeInteraction state active []
  where
    choices = interactionChoices (activeScript active)
    dialogue = interactionDialogue (activeScript active)
    index = activeDialogueIndex active

-- | Apply an established fact without checking command rules again.
applyEvent :: GameState -> Event -> GameState
applyEvent state = \case
  Event.BattleChanged update -> state{battle = Just (updateBattle update), inventory = fromMaybe (inventory state) (updateInventory update), party = fromMaybe (party state) (updateParty update), randomSeed = fromMaybe (randomSeed state) (updateSeed update)}
  Event.BattleEnded -> state{battle = Nothing}
  Event.BattleStarted active -> state{battle = Just active}
  Event.ConversationAdvanced index -> state{conversation = fmap (\active -> active{lineIndex = index}) (conversation state)}
  Event.ConversationEnded -> state{conversation = Nothing}
  Event.ConversationStarted identifier -> state{conversation = Just (Conversation 0 identifier)}
  Event.CritterReceived critter -> state{party = Party (members (party state) <> [critter])}
  Event.EncounterAdvanced count -> state{encounterSteps = count}
  Event.FlagChanged identifier value -> state{flags = Map.insert identifier value (flags state)}
  Event.InteractionAdvanced index -> state{activeInteraction = fmap (\active -> active{activeDialogueIndex = index}) (activeInteraction state)}
  Event.InteractionChoiceOpened -> state{activeInteraction = fmap (\active -> active{activeSelectedChoice = Just 0}) (activeInteraction state)}
  Event.InteractionChoiceSelected index -> state{activeInteraction = fmap (\active -> active{activeSelectedChoice = Just index}) (activeInteraction state)}
  Event.InteractionEnded -> state{activeInteraction = Nothing}
  Event.InteractionStarted active -> state{activeInteraction = Just active}
  Event.InventoryChanged identifier count -> state{inventory = Map.insert identifier count (inventory state)}
  Event.ObjectRemoved identifier -> state{objects = Map.delete identifier (objects state)}
  Event.PartyChanged value -> state{party = value}
  Event.PartyClosed -> state{partyOpen = False}
  Event.PartyOpened -> state{partyOpen = True}
  Event.PlayerMoved destination -> state{playerPosition = destination}
  Event.PlayerTurned direction -> state{playerDirection = direction}
  Event.PlayerWarped destination ->
    state{conversation = Nothing, playerMap = Location.mapId destination, playerPosition = Location.position destination}
  Event.RandomAdvanced seed -> state{randomSeed = seed}

canStand :: GameState -> Position -> Bool
canStand state destination =
  maybe False walkable (tileAt (world state) destination)
    && not (occupied state destination)

-- | Select the first branch whose conditions all hold in the current state.
chooseInteraction :: GameState -> [Interaction] -> Maybe Interaction
chooseInteraction state = find (all (conditionHolds state) . interactionConditions)

-- | The active view determines what Enter, Escape and movement mean.
commandFor :: GameState -> Input -> Maybe Command
commandFor state input
  | input == Input.Character 'q' = Just Quit
  | partyOpen state = case input of
      Input.Character 'p' -> Just CloseMenu
      Input.Escape -> Just CloseMenu
      _ -> Nothing
  | isJust (battle state) = BattleEngine.commandFor input
  | Just active <- activeInteraction state = case activeSelectedChoice active of
      Just _ -> case input of
        Input.Character 'j' -> Just SelectNext
        Input.Character 'k' -> Just SelectPrevious
        Input.Down -> Just SelectNext
        Input.Enter -> Just ConfirmSelection
        Input.Escape -> Just CloseMenu
        Input.Up -> Just SelectPrevious
        _ -> Nothing
      Nothing -> case input of
        Input.Enter -> Just AdvanceConversation
        Input.Escape -> Just EndConversation
        _ -> Nothing
  | otherwise = case (conversation state, input) of
      (Just _, Input.Enter) -> Just AdvanceConversation
      (Just _, Input.Escape) -> Just EndConversation
      (Just _, _) -> Nothing
      (Nothing, Input.Character 'h') -> Just (Walk West)
      (Nothing, Input.Character 'j') -> Just (Walk South)
      (Nothing, Input.Character 'k') -> Just (Walk North)
      (Nothing, Input.Character 'l') -> Just (Walk East)
      (Nothing, Input.Character 'p') -> Just OpenParty
      (Nothing, Input.Down) -> Just (Walk South)
      (Nothing, Input.Enter) -> Just Interact
      (Nothing, Input.Escape) -> Just CloseMenu
      (Nothing, Input.Left) -> Just (Walk West)
      (Nothing, Input.Right) -> Just (Walk East)
      (Nothing, Input.Up) -> Just (Walk North)
      _ -> Nothing

completeInteraction :: GameState -> ActiveInteraction -> [Effect] -> [Event]
completeInteraction state active extra =
  case eventsForEffects state (interactionEffects (activeScript active) <> extra) of
    Left message -> startInteraction state (Interaction [] [] [message] [])
    Right events -> events <> [Event.InteractionEnded]

-- | Conditions inspect current state; a missing flag has the value false.
conditionHolds :: GameState -> Condition -> Bool
conditionHolds state (Flag identifier expected) = Map.findWithDefault False identifier (flags state) == expected

-- | Only valid destinations produce movement facts. Ledges are crossed in one
-- input, landing one cell beyond them in their allowed direction.
eventsFor :: GameState -> Command -> [Event]
eventsFor state command
  | partyOpen state = [Event.PartyClosed | command == CloseMenu]
  | Just active <- battle state =
      if command == ConfirmSelection && Battle.battlePhase active == Finished then finishBattle state active else BattleEngine.eventsFor state command
  | Just active <- activeInteraction state = interactionEvents state active command
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
      Interact | isNothing (conversation state) -> interactEvents state
      OpenParty | isNothing (conversation state) -> [Event.PartyOpened]
      Walk direction
        | isNothing (conversation state) ->
            let adjacent = move direction (playerPosition state)
                destination = case tileAt (world state) adjacent of
                  Just (Ledge allowed) | allowed == direction -> move direction adjacent
                  _ -> adjacent
                turns = [Event.PlayerTurned direction | direction /= playerDirection state]
             in turns
                  <> if not (occupied state adjacent) then movementEvents state direction destination else []
      _ -> []

-- | Validate the entire batch against a temporary state before returning facts.
-- A later failure discards every earlier effect in the batch.
eventsForEffects :: GameState -> [Effect] -> Either Text [Event]
eventsForEffects state = fmap snd . foldM apply (state, [])
  where
    apply (current, events) effect = do
      event <- case effect of
        Effect.GiveCritter critter
          | length (members (party current)) < maxPartySize current -> Right (Event.CritterReceived critter)
          | otherwise -> Left "Your party is full."
        Effect.GiveItem identifier count
          | count > 0 && Map.member identifier (Config.configItems (combat current)) && count <= maxBound - Map.findWithDefault 0 identifier (inventory current) -> Right (Event.InventoryChanged identifier (Map.findWithDefault 0 identifier (inventory current) + count))
          | otherwise -> Left "That item or count is invalid."
        Effect.HealParty -> Right (Event.PartyChanged (BattleCommon.healParty 1 (party current)))
        Effect.RemoveObject identifier
          | Map.member identifier (objects current) -> Right (Event.ObjectRemoved identifier)
          | otherwise -> Left "That object is no longer available."
        Effect.SetFlag identifier value -> Right (Event.FlagChanged identifier value)
        Effect.StartBattle identifier -> Event.BattleStarted <$> BattleEngine.beginBattle current identifier
      pure (applyEvent current event, events <> [event])

finishBattle :: GameState -> Battle -> [Event]
finishBattle state active = case eventsForEffects closed (BattleEngine.completionEffects active) of
  Left message -> Event.BattleEnded : startInteraction closed (Interaction [] [] [message] [])
  Right effects -> Event.BattleEnded : effects
  where
    closed = applyEvent state Event.BattleEnded

interactEvents :: GameState -> [Event]
interactEvents state = case GameState.npcAt state facing of
  Just (identifier, npc)
    | not (null (Npc.interactions npc)) -> maybe [] (startInteraction state) (chooseInteraction state (Npc.interactions npc))
    | not (null (Npc.dialogue npc)) -> [Event.ConversationStarted identifier]
    | otherwise -> []
  Nothing -> case GameState.objectAt state facing of
    Just (_, object) -> maybe [] (startInteraction state) (chooseInteraction state (Object.objectInteractions object))
    Nothing -> []
  where
    facing = move (playerDirection state) (playerPosition state)

interactionEvents :: GameState -> ActiveInteraction -> Command -> [Event]
interactionEvents state active = \case
  AdvanceConversation -> advanceInteraction state active
  CloseMenu -> [Event.InteractionEnded]
  ConfirmSelection -> case activeSelectedChoice active of
    Just index | index >= 0 -> case drop index (interactionChoices (activeScript active)) of
      choice : _ -> completeInteraction state active (Choice.choiceEffects choice)
      [] -> []
    _ -> []
  EndConversation -> [Event.InteractionEnded]
  SelectNext -> selectChoice 1 active
  SelectPrevious -> selectChoice (-1) active
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

occupied :: GameState -> Position -> Bool
occupied state position = isJust (GameState.npcAt state position) || isJust (GameState.objectAt state position)

selectChoice :: Int -> ActiveInteraction -> [Event]
selectChoice delta active = case activeSelectedChoice active of
  Just index
    | index >= 0 && index < count ->
        let next = max 0 (min (count - 1) (index + delta))
         in [Event.InteractionChoiceSelected next | next /= index]
  _ -> []
  where
    count = length (interactionChoices (activeScript active))

startInteraction :: GameState -> Interaction -> [Event]
startInteraction state script
  | null (interactionDialogue script) && null (interactionChoices script) =
      case eventsForEffects state (interactionEffects script) of
        Left message -> startInteraction state (Interaction [] [] [message] [])
        Right events -> events
  | otherwise =
      [ Event.InteractionStarted
          (ActiveInteraction 0 script (if length (interactionDialogue script) <= 1 && not (null (interactionChoices script)) then Just 0 else Nothing))
      ]

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
