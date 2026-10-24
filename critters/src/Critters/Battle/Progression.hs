module Critters.Battle.Progression (experienceAt, rewardDefeated) where

import Control.Monad (forM_, when)
import Control.Monad.Trans.State.Strict (State)
import Control.Monad.Trans.State.Strict qualified as State
import Critters.Battle.Common (Context (..), Side (..))
import Critters.Battle.Common qualified as Common
import Critters.Number (integer, number)
import Critters.State.Battle (Battle (..))
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
experienceAt rules level = max 0 (integer (Common.evaluate 0 (Rules.rulesExperienceFormula rules) [("level", number level)]))

gain :: Int -> Int -> State Context ()
gain index amount = when (amount > 0) $ do
  context <- State.get
  case drop index (Common.allCritters Player context) of
    critter : _ -> do
      let rules = Common.rulesFor context
          profile = Critter.battleProfile critter
          total = max (Profile.profileExperience profile) (min (experienceAt rules (Rules.rulesMaxLevel rules)) (integer (number (max (experienceAt rules (Critter.level critter)) (Profile.profileExperience profile)) + number amount)))
          newLevel = foldl' (\current candidate -> if total >= experienceAt rules candidate then candidate else current) (Critter.level critter) [Critter.level critter + 1 .. Rules.rulesMaxLevel rules]
          progressed = learn context critter{Critter.battleProfile = profile{Profile.profileExperience = total}} newLevel
      State.modify (Common.setCritter Player index progressed)
      Common.emit (Critter.species critter <> " gained " <> from (show amount) <> " experience.")
      when (newLevel > Critter.level critter) $ do
        Common.emit (Critter.species critter <> " reached level " <> from (show newLevel) <> ".")
        forM_ (filter (`notElem` Common.knownMoves (contextBattle context) critter) (Common.knownMoves (contextBattle context) progressed)) $ \identifier ->
          Common.emit (Critter.species critter <> " learned " <> maybe "a move" Move.moveName (Map.lookup identifier (Config.configMoves (battleConfig (contextBattle context)))) <> ".")
    [] -> pure ()

learn :: Context -> Critter -> Int -> Critter
learn context critter newLevel
  | newLevel <= Critter.level critter = critter
  | otherwise = updated{Critter.battleProfile = (Critter.battleProfile updated){Profile.profileKnownMoves = Common.knownMoves current updated}}
  where
    current = contextBattle context
    definition = Map.findWithDefault Species.defaultSpecies (Critter.species critter) (Config.configSpecies (battleConfig current))
    learned = concatMap snd (filter (\(level, _) -> level > Critter.level critter && level <= newLevel) (Map.toAscList (Species.speciesLearnset definition)))
    maximumHP = max 1 (integer (Common.evaluate (number (Critter.maxHitPoints critter)) (Rules.rulesHealthFormula (Common.rulesFor context)) [("base", number (Map.findWithDefault (Critter.maxHitPoints critter) "health" (Species.speciesStats definition))), ("level", number newLevel)]))
    updated =
      critter
        { Critter.battleProfile = (Critter.battleProfile critter){Profile.profileKnownMoves = nub (Common.knownMoves current critter <> learned)},
          Critter.hitPoints = if Critter.hitPoints critter == 0 then 0 else max 1 (min maximumHP (integer (number (Critter.hitPoints critter) + number maximumHP - number (Critter.maxHitPoints critter)))),
          Critter.level = newLevel,
          Critter.maxHitPoints = maximumHP
        }

rewardDefeated :: State Context ()
rewardDefeated = do
  context <- State.get
  forM_ [(index, critter) | (index, critter) <- zip [0 ..] (Common.allCritters Enemy context), not (Common.usable critter), index `notElem` battleRewardedOpponents (contextBattle context)] $ \(index, defeated) -> do
    current <- State.get
    State.modify $ \value -> value{contextBattle = (contextBattle value){battleRewardedOpponents = battleRewardedOpponents (contextBattle value) <> [index]}}
    let rules = Common.rulesFor current
        captured = index `elem` battleCapturedOpponents (contextBattle current)
        participants =
          filter
            (\slot -> Rules.rulesExperienceForFainted rules || maybe False Common.usable (case drop slot (Common.allCritters Player current) of critter : _ -> Just critter; [] -> Nothing))
            (if Rules.rulesExperienceSharing rules == "party" then [0 .. length (Party.members (contextParty current)) - 1] else battleParticipants (contextBattle current))
        yield = fromMaybe 0 (Species.speciesExperienceYield <$> Map.lookup (Critter.species defeated) (Config.configSpecies (battleConfig (contextBattle current))))
        amount = max 0 (integer (Common.evaluate 0 (Rules.rulesExperienceRewardFormula rules) [("level", number (Critter.level defeated)), ("participants", number (length participants)), ("yield", number yield)]))
    when (not captured || Rules.rulesCaptureGivesExperience rules) (mapM_ (\slot -> gain slot amount) participants)
