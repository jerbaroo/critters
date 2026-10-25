module Critters.Battle.Engine (beginBattle, beginEncounter, commandFor, completionEffects, eventsFor) where

import Critters.Battle.Action (Action (SwitchCritter))
import Critters.Battle.Common (Context (..))
import Critters.Battle.Common qualified as Common
import Critters.Battle.Menu qualified as Menu
import Critters.Battle.Turn qualified as Turn
import Critters.Command (Command (..))
import Critters.Event (Event)
import Critters.Event qualified as Event
import Critters.Input (Input)
import Critters.Input qualified as Input
import Critters.Input.Direction (Direction (..))
import Critters.Message qualified as Message
import Critters.Number qualified as Number
import Critters.Selection qualified as Selection
import Critters.State.Battle (Battle (..), defaultBattle)
import Critters.State.Battle qualified as Battle
import Critters.State.BattleDefinition (BattleDefinition)
import Critters.State.BattleDefinition qualified as Definition
import Critters.State.BattleOutcome (BattleOutcome (..))
import Critters.State.BattlePhase (BattlePhase (..))
import Critters.State.CombatConfig qualified as Config
import Critters.State.Critter qualified as Critter
import Critters.State.Effect (Effect)
import Critters.State.EncounterEntry (EncounterEntry)
import Critters.State.EncounterEntry qualified as Entry
import Critters.State.GameState (GameState (..))
import Critters.State.GameState qualified as GameState
import Critters.State.Message (BattleMessage (..), Presentation (..))
import Critters.State.Party qualified as Party
import Critters.State.Rules qualified as Rules
import Critters.State.Species qualified as Species
import Data.List (findIndex)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as Text

beginBattle :: GameState -> Text -> Either Text Battle
beginBattle state identifier =
    maybe (Left (GameState.messageText state "battleUndefined" [])) (beginDefinition state) (Map.lookup identifier state.combat.battles)

beginEncounter :: GameState -> EncounterEntry -> Either Text Battle
beginEncounter state entry = do
    species <- maybe (Left (GameState.messageText state "encounterSpeciesUndefined" [])) Right (Map.lookup entry.species state.combat.species)
    let level = entry.level
        baseHealth = Map.findWithDefault (Map.findWithDefault 20 "health" (((Species.defaultSpecies).stats).values)) "health" species.stats.values
        hitPoints = max 1 $ Number.integer $ Common.evaluate state.combat.rules.healthFormula [("base", Number.number baseHealth), ("level", Number.number level)]
        displayName = if Text.null (Text.strip species.name) then entry.species else species.name
        definition =
            Definition.defaultDefinition
                { Definition.allowCapture = entry.allowCapture
                , Definition.allowEscape = entry.allowEscape
                , Definition.name = GameState.messageText state "battleWildName" [("species", Message.text displayName)]
                , Definition.opponents = [Critter.basicCritter hitPoints level hitPoints entry.species]
                }
    beginDefinition state definition

beginDefinition :: GameState -> BattleDefinition -> Either Text Battle
beginDefinition state definition
    | isJust state.battle = Left (GameState.messageText state "battleAlreadyActive" [])
    | state.partyOpen = Left (GameState.messageText state "battleCloseParty" [])
    | otherwise = do
        player <- maybe (Left (GameState.messageText state "battleNoConsciousCritter" [])) Right (findIndex Common.usable state.party.members)
        opponent <- maybe (Left (GameState.messageText state "battleNoConsciousOpponent" [])) Right (findIndex Common.usable definition.opponents)
        if length definition.opponents > state.maxPartySize
            then Left (GameState.messageText state "battleOpponentPartyTooLarge" [])
            else
                if all (Common.validCritter state.combat) (state.party.members <> definition.opponents)
                    then
                        Right
                            defaultBattle
                                { Battle.activeOpponent = opponent
                                , Battle.activePlayer = player
                                , Battle.config = state.combat
                                , Battle.definition = definition
                                , Battle.initialSeed = Just state.randomSeed
                                , Battle.messages = [BattleMessage (Message.named "battleTitle" [("opponent", Message.reference definition.name)]) Dialogue]
                                , Battle.messageTemplates = state.messages
                                , Battle.opponentParty = definition.opponents
                                , Battle.participants = [player]
                                , Battle.phase = if null definition.introduction then ActionMenu else Introduction
                                , Battle.rewardedOpponents = [index | (index, critter) <- zip [0 ..] definition.opponents, not (Common.usable critter)]
                                }
                    else Left (GameState.messageText state "battleInvalidParticipant" [])

commandFor :: Input -> Maybe Command
commandFor = \case
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

completionEffects :: Battle -> [Effect]
completionEffects active = case active.outcome of
    Just Captured -> active.definition.captureEffects
    Just Defeat -> active.definition.defeatEffects
    Just Escaped -> active.definition.escapeEffects
    Just Victory -> active.definition.victoryEffects
    Nothing -> []

eventsFor :: GameState -> Command -> [Event]
eventsFor state command = case Common.contextFor state of
    Nothing -> []
    Just context | context.battle.phase == Introduction ->
        case command of
            ConfirmSelection
                | context.battle.introductionIndex + 1 < length context.battle.definition.introduction -> [Event.BattleIntroductionAdvanced (context.battle.introductionIndex + 1)]
                | otherwise -> [Event.BattleCombatantsSentOut]
            _ -> []
    Just context -> case command of
        ActInBattle action -> Turn.resolveAction state action
        CloseMenu | active.phase `notElem` [ActionMenu, Finished, ReplacementMenu] -> update context active{Battle.phase = ActionMenu, Battle.selected = 0, Battle.selectedItem = Nothing}
        ConfirmSelection -> case drop active.selected (Menu.entries state) of
            (_, Menu.GoTo phase) : _ -> update context active{Battle.phase = phase, Battle.selected = 0}
            (_, Menu.Perform (SwitchCritter index)) : _
                | active.phase == SwitchMenu && index == active.activePlayer -> update context active{Battle.phase = ActionMenu, Battle.selected = 0}
            (_, Menu.Perform action) : _ -> Turn.resolveAction state action
            (_, Menu.PickItem identifier) : _ -> update context active{Battle.phase = ItemTargetMenu, Battle.selected = 0, Battle.selectedItem = Just identifier}
            _ -> []
        Select direction -> update context active{Battle.selected = Selection.moveSelection columns (length (Menu.entries state)) direction active.selected}
        SelectNext -> select context 1
        SelectPrevious -> select context (-1)
        _ -> []
      where
        active = context.battle
        columns = if active.phase `elem` [ItemTargetMenu, ReplacementMenu, SwitchMenu] then 1 else 2
        select current delta = update current active{Battle.selected = max 0 (min (length (Menu.entries state) - 1) (active.selected + delta))}
        update current next = Common.changedEvents state current{Common.battle = next}
