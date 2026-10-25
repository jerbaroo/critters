-- | Pure pipeline from Input -> Command -> Event -> State.
module Critters.Engine (applyEvent, chooseInteraction, commandFor, conditionHolds, eventsFor, eventsForEffects, step) where

import Control.Monad (foldM)
import Critters.Battle.Common qualified as BattleCommon
import Critters.Battle.Encounter qualified as Encounter
import Critters.Battle.Engine qualified as BattleEngine
import Critters.Command (Command (..))
import Critters.Event (Event)
import Critters.Event qualified as Event
import Critters.Input (Input)
import Critters.Input qualified as Input
import Critters.Input.Direction (Direction (..))
import Critters.Selection qualified as Selection
import Critters.State.ActiveInteraction (ActiveInteraction (..))
import Critters.State.ActiveInteraction qualified as ActiveInteraction
import Critters.State.Battle (Battle)
import Critters.State.Battle qualified as Battle
import Critters.State.BattlePhase (BattlePhase (ActionMenu, Finished))
import Critters.State.BattleUpdate (BattleUpdate (..))
import Critters.State.Choice qualified as Choice
import Critters.State.CombatConfig qualified as Config
import Critters.State.Condition (Condition (..))
import Critters.State.Conversation (Conversation (..))
import Critters.State.Conversation qualified as Conversation
import Critters.State.Effect (Effect)
import Critters.State.Effect qualified as Effect
import Critters.State.GameState (GameState (..), world)
import Critters.State.GameState qualified as GameState
import Critters.State.Interaction (Interaction (..))
import Critters.State.Location qualified as Location
import Critters.State.Npc qualified as Npc
import Critters.State.NpcId (NpcId)
import Critters.State.Object qualified as Object
import Critters.State.Party (Party (..))
import Critters.State.Position (Position (..))
import Critters.State.Rules qualified as Rules
import Critters.State.Tile (Tile (..))
import Critters.State.Warp qualified as Warp
import Critters.State.World (World (..), tileAt)
import Data.List (find)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Text (Text)
import Witch (from)

advanceInteraction :: GameState -> ActiveInteraction -> [Event]
advanceInteraction state active
    | isJust active.selectedChoice = []
    | index < 0 || index >= length dialogue = [Event.InteractionEnded]
    | index < length dialogue - 1 =
        [Event.InteractionAdvanced (index + 1)]
            <> [Event.InteractionChoiceOpened | index + 1 == length dialogue - 1 && not (null choices)]
    | not (null choices) = [Event.InteractionChoiceOpened]
    | otherwise = completeInteraction state active []
  where
    choices = active.script.choices
    dialogue = active.script.dialogue
    index = active.dialogueIndex

applyBattleUpdate :: GameState -> BattleUpdate -> GameState
applyBattleUpdate state update = state{GameState.battle = Just update.battle, GameState.inventory = fromMaybe state.inventory update.inventory, GameState.party = fromMaybe state.party update.party, GameState.randomSeed = fromMaybe state.randomSeed update.seed}

-- | Apply an established fact without checking command rules again.
applyEvent :: GameState -> Event -> GameState
applyEvent state = \case
    Event.BattleChanged update -> applyBattleUpdate state update
    Event.BattleCombatantsSentOut -> state{GameState.battle = fmap (\active -> active{Battle.phase = ActionMenu}) state.battle}
    Event.BattleEnded -> state{GameState.battle = Nothing}
    Event.BattleIntroductionAdvanced index -> state{GameState.battle = fmap (\active -> active{Battle.introductionIndex = index}) state.battle}
    Event.BattleStarted active -> state{GameState.battle = Just active, GameState.randomSeed = fromMaybe state.randomSeed active.initialSeed}
    Event.ConversationAdvanced index -> state{GameState.conversation = fmap (\active -> active{Conversation.lineIndex = index}) state.conversation}
    Event.ConversationEnded -> state{GameState.conversation = Nothing}
    Event.ConversationStarted identifier -> state{GameState.conversation = Just (Conversation 0 identifier)}
    Event.CritterReceived critter -> state{GameState.party = Party (state.party.members <> [critter])}
    Event.EncounterAdvanced count -> state{GameState.encounterSteps = count}
    Event.FlagChanged identifier value -> state{GameState.flags = Map.insert identifier value state.flags}
    Event.InteractionAdvanced index -> state{GameState.activeInteraction = fmap (\active -> active{ActiveInteraction.dialogueIndex = index}) state.activeInteraction}
    Event.InteractionChoiceOpened -> state{GameState.activeInteraction = fmap (\active -> active{ActiveInteraction.selectedChoice = Just 0}) state.activeInteraction}
    Event.InteractionChoiceSelected index -> state{GameState.activeInteraction = fmap (\active -> active{ActiveInteraction.selectedChoice = Just index}) state.activeInteraction}
    Event.InteractionEnded -> state{GameState.activeInteraction = Nothing}
    Event.InteractionStarted active -> state{GameState.activeInteraction = Just active}
    Event.InventoryChanged identifier count -> state{GameState.inventory = Map.insert identifier count state.inventory}
    Event.MoveUsed _ _ update -> applyBattleUpdate state update
    Event.NpcMoved identifier destination -> state{GameState.npcs = Map.adjust (\npc -> npc{Npc.location = destination}) identifier state.npcs}
    Event.ObjectRemoved identifier -> state{GameState.objects = Map.delete identifier state.objects}
    Event.PartyChanged value -> state{GameState.party = value}
    Event.PartyClosed -> state{GameState.partyOpen = False}
    Event.PartyOpened -> state{GameState.partyOpen = True}
    Event.PlayerMoved destination -> state{GameState.playerPosition = destination}
    Event.PlayerTurned direction -> state{GameState.playerDirection = direction}
    Event.PlayerWarped destination ->
        state{GameState.conversation = Nothing, GameState.playerMap = destination.mapId, GameState.playerPosition = destination.position}
    Event.RandomAdvanced seed -> state{GameState.randomSeed = seed}
    Event.TurnCompleted update -> applyBattleUpdate state update

canStand :: GameState -> Position -> Bool
canStand state destination =
    maybe False walkable (tileAt (world state) destination)
        && not (occupied state destination)

-- | Select the first branch whose conditions all hold in the current state.
chooseInteraction :: GameState -> [Interaction] -> Maybe Interaction
chooseInteraction state = find (all (conditionHolds state) . (.conditions))

-- | The active view determines what Enter, Escape and movement mean.
commandFor :: GameState -> Input -> Maybe Command
commandFor state input
    | input == Input.Character 'q' = Just Quit
    | state.partyOpen = case input of
        Input.Character 'p' -> Just CloseMenu
        Input.Escape -> Just CloseMenu
        _ -> Nothing
    | isJust state.battle = BattleEngine.commandFor input
    | Just active <- state.activeInteraction = case active.selectedChoice of
        Just _ -> case input of
            Input.Character 'h' -> Just (Select West)
            Input.Character 'j' -> Just (Select South)
            Input.Character 'k' -> Just (Select North)
            Input.Character 'l' -> Just (Select East)
            Input.Down -> Just (Select South)
            Input.Enter -> Just ConfirmSelection
            Input.Escape -> Just CloseMenu
            Input.Left -> Just (Select West)
            Input.Right -> Just (Select East)
            Input.Up -> Just (Select North)
            _ -> Nothing
        Nothing -> case input of
            Input.Enter -> Just AdvanceConversation
            Input.Escape -> Just EndConversation
            _ -> Nothing
    | otherwise = case (state.conversation, input) of
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
    case eventsForEffects state (active.script.effects <> extra) of
        Left message -> startInteraction state (Interaction [] [] [message] [])
        Right events -> events <> [Event.InteractionEnded]

-- | Conditions inspect current state; a missing flag has the value false.
conditionHolds :: GameState -> Condition -> Bool
conditionHolds state (Flag identifier expected) = Map.findWithDefault False identifier state.flags == expected

{- | Only valid destinations produce movement facts. Ledges are crossed in one
input, landing one cell beyond them in their allowed direction.
-}
eventsFor :: GameState -> Command -> [Event]
eventsFor state command
    | state.partyOpen = [Event.PartyClosed | command == CloseMenu]
    | Just active <- state.battle =
        if command == ConfirmSelection && active.phase == Finished
            then finishBattle state active
            else
                let events = BattleEngine.eventsFor state command
                    resolved = foldl' applyEvent state events
                 in events <> case resolved.battle of
                        Just completed | not (null events) && completed.phase == Finished -> finishBattle resolved completed
                        _ -> []
    | Just active <- state.activeInteraction = interactionEvents state active command
    | otherwise = case command of
        AdvanceConversation -> case state.conversation of
            Just active -> case Map.lookup active.npcId state.npcs of
                Just npc
                    | npc.location.mapId == state.playerMap
                        && active.lineIndex >= 0
                        && active.lineIndex < length npc.dialogue - 1 ->
                        [Event.ConversationAdvanced (active.lineIndex + 1)]
                _ -> [Event.ConversationEnded]
            Nothing -> []
        EndConversation -> [Event.ConversationEnded | isJust state.conversation]
        Interact | isNothing state.conversation -> interactEvents state
        OpenParty | isNothing state.conversation -> [Event.PartyOpened]
        Walk direction
            | isNothing state.conversation ->
                let adjacent = move direction state.playerPosition
                    destination = case tileAt (world state) adjacent of
                        Just (Ledge allowed) | allowed == direction -> move direction adjacent
                        _ -> adjacent
                    turns = [Event.PlayerTurned direction | direction /= state.playerDirection]
                    facts = turns <> if not (occupied state adjacent) then movementEvents state direction destination else []
                    moved = foldl' applyEvent state facts
                    entered = any (\case Event.PlayerMoved _ -> True; _ -> False) facts
                    warped = any (\case Event.PlayerWarped _ -> True; _ -> False) facts
                 in facts <> if entered && (not warped || state.combat.rules.encountersAfterWarp) then Encounter.eventsForStep moved else []
        _ -> []

{- | Validate the entire batch against a temporary state before returning facts.
A later failure discards every earlier effect in the batch.
-}
eventsForEffects :: GameState -> [Effect] -> Either Text [Event]
eventsForEffects state = fmap snd . foldM apply (state, [])
  where
    apply (current, events) (Effect.WalkNpc identifier path) = do
        steps <- walkNpc current identifier path
        pure (foldl' applyEvent current steps, events <> steps)
    apply (current, events) effect = do
        event <- case effect of
            Effect.GiveCritter critter
                | not (Map.null current.combat.species) && not (BattleCommon.validCritter current.combat critter) -> Left "That critter has invalid combat data."
                | length current.party.members < current.maxPartySize -> Right (Event.CritterReceived critter)
                | otherwise -> Left "Your party is full."
            Effect.GiveItem identifier count
                | count > 0 && Map.member identifier current.combat.items && count <= maxBound - Map.findWithDefault 0 identifier current.inventory -> Right (Event.InventoryChanged identifier (Map.findWithDefault 0 identifier current.inventory + count))
                | otherwise -> Left "That item or count is invalid."
            Effect.HealParty -> Right (Event.PartyChanged (BattleCommon.healParty 1 current.party))
            Effect.RemoveObject identifier
                | Map.member identifier current.objects -> Right (Event.ObjectRemoved identifier)
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
        | not (null npc.interactions) -> maybe [] (startInteraction state) (chooseInteraction state npc.interactions)
        | not (null npc.dialogue) -> [Event.ConversationStarted identifier]
        | otherwise -> []
    Nothing -> case GameState.objectAt state facing of
        Just (_, object) -> maybe [] (startInteraction state) (chooseInteraction state object.interactions)
        Nothing -> []
  where
    facing = move state.playerDirection state.playerPosition

interactionEvents :: GameState -> ActiveInteraction -> Command -> [Event]
interactionEvents state active = \case
    AdvanceConversation -> advanceInteraction state active
    CloseMenu -> [Event.InteractionEnded]
    ConfirmSelection -> case active.selectedChoice of
        Just index | index >= 0 -> case drop index active.script.choices of
            choice : _ -> completeInteraction state active choice.effects
            [] -> []
        _ -> []
    EndConversation -> [Event.InteractionEnded]
    Select direction -> selectChoice (\count -> Selection.moveSelection 2 count direction) active
    SelectNext -> selectChoice (\count index -> min (count - 1) (index + 1)) active
    SelectPrevious -> selectChoice (\_ index -> max 0 (index - 1)) active
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
    | otherwise = case find ((== destination) . (.source)) ((world state).warps) of
        Nothing -> [Event.PlayerMoved destination]
        Just warp
            | maybe True (== direction) warp.entranceDirection ->
                let target = warp.destination
                    arrived = state{GameState.playerMap = target.mapId, GameState.playerPosition = target.position}
                 in if canStand arrived target.position
                        then [Event.PlayerMoved destination, Event.PlayerWarped target]
                        else []
        _ -> []

occupied :: GameState -> Position -> Bool
occupied state position = isJust (GameState.npcAt state position) || isJust (GameState.objectAt state position)

selectChoice :: (Int -> Int -> Int) -> ActiveInteraction -> [Event]
selectChoice selection active = case active.selectedChoice of
    Just index
        | index >= 0 && index < count ->
            let next = selection count index
             in [Event.InteractionChoiceSelected next | next /= index]
    _ -> []
  where
    count = length active.script.choices

startInteraction :: GameState -> Interaction -> [Event]
startInteraction state script
    | null script.dialogue && null script.choices =
        case eventsForEffects state script.effects of
            Left message -> startInteraction state (Interaction [] [] [message] [])
            Right events -> events
    | otherwise =
        [ Event.InteractionStarted
            (ActiveInteraction 0 script (if length script.dialogue <= 1 && not (null script.choices) then Just 0 else Nothing))
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

walkNpc :: GameState -> NpcId -> [Position] -> Either Text [Event]
walkNpc state identifier path = snd <$> foldM stepNpc (state, []) path
  where
    stepNpc (current, events) destination = do
        npc <- maybe (Left "That NPC is not available.") Right (Map.lookup identifier current.npcs)
        let origin = npc.location
            Position oldX oldY = origin.position
            Position newX newY = destination
            onMap = current{GameState.playerMap = origin.mapId}
            playerBlocks = current.playerMap == origin.mapId && current.playerPosition == destination
        if abs (from @Int @Integer newX - from @Int @Integer oldX) + abs (from @Int @Integer newY - from @Int @Integer oldY) /= 1 || playerBlocks || not (canStand onMap destination)
            then Left "That NPC walking path is blocked or has a nonadjacent step."
            else
                let event = Event.NpcMoved identifier origin{Location.position = destination}
                 in Right (applyEvent current event, events <> [event])
