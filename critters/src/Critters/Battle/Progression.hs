module Critters.Battle.Progression (experienceAt, rewardDefeated) where

import Control.Monad (forM_, when)
import Control.Monad.Trans.State.Strict (State)
import Control.Monad.Trans.State.Strict qualified as State
import Critters.Battle.Common (Context (..), Side (..))
import Critters.Battle.Common qualified as Common
import Critters.Number (integer, number)
import Critters.State.Battle (Battle (..))
import Critters.State.Battle qualified as Battle
import Critters.State.BattleProfile qualified as Profile
import Critters.State.CombatConfig qualified as Config
import Critters.State.Critter (Critter)
import Critters.State.Critter qualified as Critter
import Critters.State.Move qualified as Move
import Critters.State.Party qualified as Party
import Critters.State.Rules (Rules)
import Critters.State.Rules qualified as Rules
import Critters.State.Species qualified as Species
import Data.List (nub)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Witch (from)

experienceAt :: Rules -> Int -> Int
experienceAt rules level = max 0 (integer (Common.evaluate rules.experienceFormula [("level", number level)]))

gain :: Int -> Int -> State Context ()
gain index amount = when (amount > 0) $ do
    context <- State.get
    case drop index (Common.allCritters Player context) of
        critter : _ -> do
            let rules = Common.rulesFor context
                profile = critter.battleProfile
                total = max profile.experience (min (experienceAt rules rules.maxLevel) (integer (number (max (experienceAt rules critter.level) profile.experience) + number amount)))
                newLevel = foldl' (\current candidate -> if total >= experienceAt rules candidate then candidate else current) critter.level [critter.level + 1 .. rules.maxLevel]
                progressed = learn context critter{Critter.battleProfile = profile{Profile.experience = total}} newLevel
            State.modify (Common.setCritter Player index progressed)
            Common.emit (critter.species <> " gained " <> from (show amount) <> " experience.")
            when (newLevel > critter.level) $ do
                Common.emit (critter.species <> " reached level " <> from (show newLevel) <> ".")
                forM_ (filter (`notElem` Common.knownMoves context.battle critter) (Common.knownMoves context.battle progressed)) $ \identifier ->
                    Common.emit (critter.species <> " learned " <> maybe "a move" (.name) (Map.lookup identifier context.battle.config.moves) <> ".")
        [] -> pure ()

learn :: Context -> Critter -> Int -> Critter
learn context critter newLevel
    | newLevel <= critter.level = critter
    | otherwise = updated{Critter.battleProfile = updated.battleProfile{Profile.knownMoves = Common.knownMoves current updated}}
  where
    current = context.battle
    definition = Map.findWithDefault Species.defaultSpecies critter.species current.config.species
    learned = concatMap snd (filter (\(level, _) -> level > critter.level && level <= newLevel) (Map.toAscList definition.learnset))
    maximumHP = max 1 (integer (Common.evaluate ((Common.rulesFor context).healthFormula) [("base", number (Map.findWithDefault critter.maxHitPoints "health" definition.stats.values)), ("level", number newLevel)]))
    updated =
        critter
            { Critter.battleProfile = critter.battleProfile{Profile.knownMoves = nub (Common.knownMoves current critter <> learned)}
            , Critter.hitPoints = if critter.hitPoints == 0 then 0 else max 1 (min maximumHP (integer (number critter.hitPoints + number maximumHP - number critter.maxHitPoints)))
            , Critter.level = newLevel
            , Critter.maxHitPoints = maximumHP
            }

rewardDefeated :: State Context ()
rewardDefeated = do
    context <- State.get
    forM_ [(index, critter) | (index, critter) <- zip [0 ..] (Common.allCritters Enemy context), not (Common.usable critter), index `notElem` context.battle.rewardedOpponents] $ \(index, defeated) -> do
        current <- State.get
        State.modify $ \value -> value{Common.battle = value.battle{Battle.rewardedOpponents = value.battle.rewardedOpponents <> [index]}}
        let rules = Common.rulesFor current
            captured = index `elem` current.battle.capturedOpponents
            participants =
                filter
                    (\slot -> rules.experienceForFainted || maybe False Common.usable (case drop slot (Common.allCritters Player current) of critter : _ -> Just critter; [] -> Nothing))
                    (if rules.experienceSharing == "party" then [0 .. length current.party.members - 1] else current.battle.participants)
            yield = fromMaybe 0 ((.experienceYield) <$> Map.lookup defeated.species current.battle.config.species)
            amount = max 0 (integer (Common.evaluate rules.experienceRewardFormula [("level", number defeated.level), ("participants", number (length participants)), ("yield", number yield)]))
        when (not captured || rules.captureGivesExperience) (mapM_ (\slot -> gain slot amount) participants)
