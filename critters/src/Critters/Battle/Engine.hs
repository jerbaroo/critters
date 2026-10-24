module Critters.Battle.Engine (beginBattle, commandFor, completionEffects, eventsFor) where

import Critters.Battle.Common qualified as Common
import Critters.Battle.Common (Context (..))
import Critters.Battle.Menu qualified as Menu
import Critters.Battle.Turn qualified as Turn
import Critters.Command (Command (..))
import Critters.Event (Event)
import Critters.Input (Input)
import Critters.Input qualified as Input
import Critters.State.Battle (Battle (..), defaultBattle)
import Critters.State.BattleDefinition qualified as Definition
import Critters.State.BattleOutcome (BattleOutcome (..))
import Critters.State.BattlePhase (BattlePhase (..))
import Critters.State.CombatConfig qualified as Config
import Critters.State.Critter qualified as Critter
import Critters.State.Effect (Effect)
import Critters.State.GameState (GameState (..))
import Critters.State.Party qualified as Party
import Data.List (findIndex)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Text (Text)

beginBattle :: GameState -> Text -> Either Text Battle
beginBattle state identifier
  | isJust (battle state) = Left "A battle is already in progress."
  | partyOpen state = Left "Close your party before starting a battle."
  | otherwise = do
      definition <- maybe (Left "That battle is not defined.") Right (Map.lookup identifier (Config.configBattles (combat state)))
      player <- maybe (Left "You need a conscious critter to battle.") Right (findIndex Common.usable (Party.members (party state)))
      opponent <- maybe (Left "There are no conscious opponents.") Right (findIndex Common.usable (Definition.definitionOpponents definition))
      if all (\critter -> Map.member (Critter.species critter) (Config.configSpecies (combat state))) (Party.members (party state) <> Definition.definitionOpponents definition)
        then Right defaultBattle
          { battleActiveOpponent = opponent,
            battleActivePlayer = player,
            battleConfig = combat state,
            battleDefinition = definition,
            battleMessages = [Definition.definitionName definition],
            battleOpponentParty = Definition.definitionOpponents definition,
            battleParticipants = [player]
          }
        else Left "A participant has no species definition."

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
