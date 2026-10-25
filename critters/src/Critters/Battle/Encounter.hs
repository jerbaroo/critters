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
import Critters.State.Battle qualified as Battle
import Critters.State.CombatConfig qualified as Config
import Critters.State.Critter (Critter)
import Critters.State.Critter qualified as Critter
import Critters.State.Encounter qualified as Encounter
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
    | isJust state.battle || not (any Common.usable state.party.members) = []
    | otherwise = case find eligible state.combat.encounters of
        Nothing -> []
        Just table
            | steps < table.minimumSteps -> [Event.EncounterAdvanced steps]
            | otherwise ->
                let initial = Context defaultBattle{Battle.config = state.combat} state.inventory state.maxPartySize state.party state.randomSeed
                    (encounter, resolved) = State.runState (select table) initial
                    randomFacts = [Event.RandomAdvanced resolved.seed | resolved.seed /= state.randomSeed]
                 in case encounter of
                        Nothing -> Event.EncounterAdvanced steps : randomFacts
                        Just active -> Event.EncounterAdvanced 0 : randomFacts <> [Event.BattleStarted active]
  where
    eligible table = table.chance > 0 && table.mapId == state.playerMap && maybe False (`elem` table.tiles) (tileAt (world state) state.playerPosition)
    select table = do
        triggered <- Common.chance table.chance
        if not triggered
            then pure Nothing
            else do
                chosen <- Common.chooseWeighted [(entry, entry.weight) | entry <- table.entries]
                case chosen of
                    Nothing -> pure Nothing
                    Just entry -> do
                        active <- case entry of
                            Entry.EncounterEntry{} -> pure (either (const Nothing) Just (BattleEngine.beginEncounter state entry))
                            Entry.LegacyEncounterEntry{Entry.battle} -> case BattleEngine.beginBattle state battle of
                                Left _ -> pure Nothing
                                Right started -> do
                                    opponents <- traverse (vary entry) started.opponentParty
                                    pure (Just started{Battle.opponentParty = opponents})
                        case active of
                            Nothing -> pure Nothing
                            Just battle -> do
                                seed <- State.gets (.seed)
                                pure (Just battle{Battle.initialSeed = Just seed})
    steps = if state.encounterSteps == maxBound then maxBound else state.encounterSteps + 1

vary :: Entry.EncounterEntry -> Critter -> State Context Critter
vary Entry.EncounterEntry{} critter = pure critter
vary Entry.LegacyEncounterEntry{Entry.maxLevel, Entry.minLevel} critter
    | minLevel == Nothing && maxLevel == Nothing = pure critter
    | otherwise = do
        context <- State.get
        let low = fromMaybe (fromMaybe critter.level maxLevel) minLevel
            high = fromMaybe low maxLevel
        level <- Common.chooseInt low high
        let definition = Map.findWithDefault Species.defaultSpecies critter.species context.battle.config.species
            health = max 1 (integer (Common.evaluate ((Common.rulesFor context).healthFormula) [("base", number (Map.findWithDefault critter.maxHitPoints "health" definition.stats.values)), ("level", number level)]))
            remaining = if critter.hitPoints == 0 then 0 else max 1 (min health (integer (number health * number critter.hitPoints / number critter.maxHitPoints)))
        pure critter{Critter.hitPoints = remaining, Critter.level = level, Critter.maxHitPoints = health}
