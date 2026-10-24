module Critters.Battle.Engine (beginBattle, beginEncounter, commandFor, completionEffects, eventsFor) where

import Critters.Battle.Common (Context (..))
import Critters.Battle.Common qualified as Common
import Critters.Battle.Menu qualified as Menu
import Critters.Battle.Turn qualified as Turn
import Critters.Command (Command (..))
import Critters.Event (Event)
import Critters.Input (Input)
import Critters.Input qualified as Input
import Critters.Number qualified as Number
import Critters.State.Battle (Battle (..), defaultBattle)
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
  maybe (Left "That battle is not defined.") (beginDefinition state) (Map.lookup identifier (Config.configBattles (combat state)))

beginEncounter :: GameState -> EncounterEntry -> Either Text Battle
beginEncounter state entry = do
  species <- maybe (Left "That encounter species is not defined.") Right (Map.lookup (Entry.entrySpecies entry) (Config.configSpecies (combat state)))
  let level = Entry.entryLevel entry
      baseHealth = Map.findWithDefault (Map.findWithDefault 20 "health" (Species.unBaseStats (Species.speciesStats Species.defaultSpecies))) "health" (Species.unBaseStats (Species.speciesStats species))
      hitPoints = max 1 $ Number.integer $ Common.evaluate (Number.number baseHealth) (Rules.rulesHealthFormula (Config.configRules (combat state))) [("base", Number.number baseHealth), ("level", Number.number level)]
      displayName = if Text.null (Text.strip (Species.speciesName species)) then Entry.entrySpecies entry else Species.speciesName species
      definition =
        Definition.defaultDefinition
          { Definition.definitionAllowCapture = Entry.entryAllowCapture entry,
            Definition.definitionAllowEscape = Entry.entryAllowEscape entry,
            Definition.definitionName = "Wild " <> displayName,
            Definition.definitionOpponents = [Critter.basicCritter hitPoints level hitPoints (Entry.entrySpecies entry)]
          }
  beginDefinition state definition

beginDefinition :: GameState -> BattleDefinition -> Either Text Battle
beginDefinition state definition
  | isJust (battle state) = Left "A battle is already in progress."
  | partyOpen state = Left "Close your party before starting a battle."
  | otherwise = do
      player <- maybe (Left "You need a conscious critter to battle.") Right (findIndex Common.usable (Party.members (party state)))
      opponent <- maybe (Left "There are no conscious opponents.") Right (findIndex Common.usable (Definition.definitionOpponents definition))
      if length (Definition.definitionOpponents definition) > maxPartySize state
        then Left "Opposing party exceeds the game party limit."
        else
          if all (Common.validCritter (combat state)) (Party.members (party state) <> Definition.definitionOpponents definition)
            then
              Right
                defaultBattle
                  { battleActiveOpponent = opponent,
                    battleActivePlayer = player,
                    battleConfig = combat state,
                    battleDefinition = definition,
                    battleInitialSeed = Just (randomSeed state),
                    battleMessages = [Definition.definitionName definition],
                    battleOpponentParty = Definition.definitionOpponents definition,
                    battleParticipants = [player],
                    battleRewardedOpponents = [index | (index, critter) <- zip [0 ..] (Definition.definitionOpponents definition), not (Common.usable critter)]
                  }
            else Left "A participant has an invalid species, move, status, resource count or level."

commandFor :: Input -> Maybe Command
commandFor = \case
  Input.Character 'j' -> Just SelectNext
  Input.Character 'k' -> Just SelectPrevious
  Input.Down -> Just SelectNext
  Input.Enter -> Just ConfirmSelection
  Input.Escape -> Just CloseMenu
  Input.Up -> Just SelectPrevious
  _ -> Nothing

completionEffects :: Battle -> [Effect]
completionEffects active = case battleOutcome active of
  Just Captured -> Definition.definitionCaptureEffects (battleDefinition active)
  Just Defeat -> Definition.definitionDefeatEffects (battleDefinition active)
  Just Escaped -> Definition.definitionEscapeEffects (battleDefinition active)
  Just Victory -> Definition.definitionVictoryEffects (battleDefinition active)
  Nothing -> []

eventsFor :: GameState -> Command -> [Event]
eventsFor state command = case Common.contextFor state of
  Nothing -> []
  Just context -> case command of
    ActInBattle action -> Turn.resolveAction state action
    CloseMenu | battlePhase active `notElem` [Finished, ReplacementMenu] -> update context active{battlePhase = ActionMenu, battleSelected = 0, battleSelectedItem = Nothing}
    ConfirmSelection -> case drop (battleSelected active) (Menu.entries state) of
      (_, Menu.GoTo phase) : _ -> update context active{battlePhase = phase, battleSelected = 0}
      (_, Menu.Perform action) : _ -> Turn.resolveAction state action
      (_, Menu.PickItem identifier) : _ -> update context active{battlePhase = ItemTargetMenu, battleSelected = 0, battleSelectedItem = Just identifier}
      _ -> []
    SelectNext -> select context 1
    SelectPrevious -> select context (-1)
    _ -> []
    where
      active = contextBattle context
      select current delta = update current active{battleSelected = max 0 (min (length (Menu.entries state) - 1) (battleSelected active + delta))}
      update current next = Common.changedEvents state current{contextBattle = next}
