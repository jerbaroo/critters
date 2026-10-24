module Critters.Battle.Encounter (eventsForStep) where

import Control.Monad.Trans.State.Strict (State)
import Control.Monad.Trans.State.Strict qualified as State
import Critters.Battle.Common (Context (..))
import Critters.Battle.Common qualified as Common
import Critters.Battle.Engine qualified as BattleEngine
import Critters.Event (Event)
import Critters.Event qualified as Event
import Critters.Number (integer, number)
import Critters.State.Battle (Battle (..), defaultBattle)
import Critters.State.CombatConfig qualified as Config
import Critters.State.Critter (Critter)
import Critters.State.Critter qualified as Critter
import Critters.State.Encounter qualified as Encounter
import Critters.State.EncounterEntry (EncounterEntry)
import Critters.State.EncounterEntry qualified as Entry
import Critters.State.GameState (GameState (..), world)
import Critters.State.Party qualified as Party
import Critters.State.Rules qualified as Rules
import Critters.State.Species qualified as Species
import Critters.State.World (tileAt)
import Data.List (find)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust)

eventsForStep :: GameState -> [Event]
eventsForStep state
  | isJust (battle state) || not (any Common.usable (Party.members (party state))) = []
  | otherwise = case find eligible (Config.configEncounters (combat state)) of
      Nothing -> []
      Just table
        | steps < Encounter.encounterMinimumSteps table -> [Event.EncounterAdvanced steps]
        | otherwise ->
            let initial = Context defaultBattle{battleConfig = combat state} (inventory state) (maxPartySize state) (party state) (randomSeed state)
                (encounter, resolved) = State.runState (select table) initial
                randomFacts = [Event.RandomAdvanced (contextSeed resolved) | contextSeed resolved /= randomSeed state]
             in case encounter of
                  Nothing -> Event.EncounterAdvanced steps : randomFacts
                  Just active -> Event.EncounterAdvanced 0 : randomFacts <> [Event.BattleStarted active]
  where
    eligible table = Encounter.encounterChance table > 0 && Encounter.encounterMapId table == playerMap state && maybe False (`elem` Encounter.encounterTiles table) (tileAt (world state) (playerPosition state))
    select table = do
      triggered <- Common.chance (Encounter.encounterChance table)
      if not triggered then pure Nothing
      else do
        chosen <- Common.chooseWeighted [(entry, Entry.entryWeight entry) | entry <- Encounter.encounterEntries table]
        case chosen of
          Nothing -> pure Nothing
          Just entry -> case BattleEngine.beginBattle state (Entry.entryBattle entry) of
            Left _ -> pure Nothing
            Right active -> do
              opponents <- traverse (vary entry) (battleOpponentParty active)
              pure (Just active{battleOpponentParty = opponents})
    steps = if encounterSteps state == maxBound then maxBound else encounterSteps state + 1

vary :: EncounterEntry -> Critter -> State Context Critter
vary entry critter
  | Entry.entryMinLevel entry == Nothing && Entry.entryMaxLevel entry == Nothing = pure critter
  | otherwise = do
      context <- State.get
      let low = fromMaybe (fromMaybe (Critter.level critter) (Entry.entryMaxLevel entry)) (Entry.entryMinLevel entry)
          high = fromMaybe low (Entry.entryMaxLevel entry)
      level <- Common.chooseInt low high
      let definition = Map.findWithDefault Species.defaultSpecies (Critter.species critter) (Config.configSpecies (battleConfig (contextBattle context)))
          health = max 1 (integer (Common.evaluate (number (Critter.maxHitPoints critter)) (Rules.rulesHealthFormula (Common.rulesFor context)) [("base", number (Map.findWithDefault (Critter.maxHitPoints critter) "health" (Species.speciesStats definition))), ("level", number level)]))
          remaining = if Critter.hitPoints critter == 0 then 0 else max 1 (min health (integer (number health * number (Critter.hitPoints critter) / number (Critter.maxHitPoints critter))))
      pure critter{Critter.hitPoints = remaining, Critter.level = level, Critter.maxHitPoints = health}
