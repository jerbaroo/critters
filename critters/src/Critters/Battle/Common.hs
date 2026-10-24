-- | Pure battle operations shared by turn resolution, menus and encounters.
module Critters.Battle.Common
  ( active, activeIndex, allCritters, availableMoves, changedEvents, chance,
    chooseInt, Context (..), contextFor, emit, evaluate, healParty, knownMoves,
    modifyActive, modifyAt, moveRemaining, opposite, randomUnit, rulesFor,
    setCritter, setStage, Side (..), stageFactor, stat, statsFor, usable
  ) where

import Control.Monad.Trans.State.Strict (State)
import Control.Monad.Trans.State.Strict qualified as State
import Critters.Event (Event)
import Critters.Event qualified as Event
import Critters.Formula (Formula)
import Critters.Formula qualified as Formula
import Critters.Number (integer, number)
import Critters.State.Affliction qualified as Affliction
import Critters.State.Battle (Battle (..))
import Critters.State.BattleProfile qualified as Profile
import Critters.State.BattleUpdate (BattleUpdate (..))
import Critters.State.CombatConfig qualified as Config
import Critters.State.Critter (Critter)
import Critters.State.Critter qualified as Critter
import Critters.State.GameState (GameState (..))
import Critters.State.Move (Move)
import Critters.State.Move qualified as Move
import Critters.State.MoveId (MoveId (..))
import Critters.State.Party (Party (..))
import Critters.State.Rules (Rules)
import Critters.State.Rules qualified as Rules
import Critters.State.Species qualified as Species
import Critters.State.StatusDefinition qualified as Status
import Data.List (nub)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Witch (from, tryFrom)

active :: Side -> Context -> Maybe Critter
active side context = case drop (activeIndex side context) (allCritters side context) of
  value : _ | activeIndex side context >= 0 -> Just value
  _ -> Nothing

activeIndex :: Side -> Context -> Int
activeIndex side context = (if side == Player then battleActivePlayer else battleActiveOpponent) (contextBattle context)

allCritters :: Side -> Context -> [Critter]
allCritters Player = members . contextParty
allCritters Enemy = battleOpponentParty . contextBattle

availableMoves :: Side -> Context -> [(MoveId, Move)]
availableMoves side context = case active side context of
  Nothing -> []
  Just critter -> case mapMaybe (available critter) (knownMoves (contextBattle context) critter) of
    [] -> [(MoveId "", Rules.rulesFallbackMove (rulesFor context))]
    moves -> moves
  where
    available critter identifier = do
      definition <- Map.lookup identifier (Config.configMoves (battleConfig (contextBattle context)))
      if Move.moveMaxUses definition == 0 || moveRemaining critter identifier definition > 0 then Just (identifier, definition) else Nothing

changedEvents :: GameState -> Context -> [Event]
changedEvents starting context
  | battle starting == Just (contextBattle context) && party starting == contextParty context && inventory starting == contextInventory context && randomSeed starting == contextSeed context = []
  | otherwise = [Event.BattleChanged (BattleUpdate (contextBattle context) (changed (inventory starting) (contextInventory context)) (changed (party starting) (contextParty context)) (changed (randomSeed starting) (contextSeed context)))]
  where
    changed old new = if old == new then Nothing else Just new

chance :: Double -> State Context Bool
chance probability
  | probability <= 0 = pure False
  | probability >= 1 = pure True
  | otherwise = (< probability) <$> randomUnit

chooseInt :: Int -> Int -> State Context Int
chooseInt low high
  | high <= low = pure low
  | otherwise = do
      value <- randomUnit
      pure (min high (low + integer (value * (number high - number low + 1))))

data Context = Context
  { contextBattle :: Battle,
    contextInventory :: Map Text Int,
    contextLimit :: Int,
    contextParty :: Party,
    contextSeed :: Int
  }
  deriving stock (Eq)

contextFor :: GameState -> Maybe Context
contextFor state = (\activeBattle -> Context activeBattle (inventory state) (maxPartySize state) (party state) (randomSeed state)) <$> battle state

emit :: Text -> State Context ()
emit message = State.modify $ \context -> context{contextBattle = (contextBattle context){battleMessages = battleMessages (contextBattle context) <> [message]}}

evaluate :: Double -> Formula -> [(Text, Double)] -> Double
evaluate fallback formula environment = either (const fallback) id (Formula.evaluate (Map.fromList environment) formula)

healParty :: Double -> Party -> Party
healParty fraction (Party critters)
  | fraction <= 0 = Party critters
  | otherwise = Party (map recover critters)
  where
    recover critter = critter
      { Critter.hitPoints = max (Critter.hitPoints critter) (max 1 (min (Critter.maxHitPoints critter) (integer (number (Critter.maxHitPoints critter) * fraction)))),
        Critter.battleProfile = (Critter.battleProfile critter){Profile.profileRemainingUses = Map.empty, Profile.profileStatus = Nothing}
      }

knownMoves :: Battle -> Critter -> [MoveId]
knownMoves current critter = limit $ nub $ case Profile.profileKnownMoves (Critter.battleProfile critter) of
  [] -> maybe [] (concatMap snd . filter ((<= Critter.level critter) . fst) . Map.toAscList . Species.speciesLearnset) (Map.lookup (Critter.species critter) (Config.configSpecies (battleConfig current)))
  moves -> moves
  where
    rules = Config.configRules (battleConfig current)
    limit moves
      | Rules.rulesLearningPolicy rules == "keep-oldest" = take (Rules.rulesMaxKnownMoves rules) moves
      | otherwise = reverse (take (Rules.rulesMaxKnownMoves rules) (reverse moves))

modifyActive :: Side -> (Critter -> Critter) -> State Context ()
modifyActive side change = State.modify $ \context -> maybe context (\critter -> setCritter side (activeIndex side context) (change critter) context) (active side context)

modifyAt :: Int -> (a -> a) -> [a] -> [a]
modifyAt index change = zipWith (\current value -> if current == index then change value else value) [0 ..]

moveRemaining :: Critter -> MoveId -> Move -> Int
moveRemaining critter identifier definition = Map.findWithDefault (Move.moveMaxUses definition) identifier (Profile.profileRemainingUses (Critter.battleProfile critter))

opposite :: Side -> Side
opposite Enemy = Player
opposite Player = Enemy

randomUnit :: State Context Double
randomUnit = do
  context <- State.get
  let next = (1103515245 * from @Int @Integer (contextSeed context) + 12345) `mod` 2147483648
      seed = either (const 1) id (tryFrom @Integer @Int next)
  State.put context{contextSeed = seed}
  pure (number seed / 2147483648)

rulesFor :: Context -> Rules
rulesFor = Config.configRules . battleConfig . contextBattle

setCritter :: Side -> Int -> Critter -> Context -> Context
setCritter Enemy index critter context = context{contextBattle = (contextBattle context){battleOpponentParty = modifyAt index (const critter) (battleOpponentParty (contextBattle context))}}
setCritter Player index critter context = context{contextParty = Party (modifyAt index (const critter) (members (contextParty context)))}

setStage :: Side -> Text -> Int -> Context -> Context
setStage side identifier delta context = context{contextBattle = updated}
  where
    current = contextBattle context
    limit = Rules.rulesStageLimit (rulesFor context)
    stages = (if side == Player then battlePlayerStages else battleEnemyStages) current
    next = Map.insert identifier (max (-limit) (min limit (Map.findWithDefault 0 identifier stages + delta))) stages
    updated = if side == Player then current{battlePlayerStages = next} else current{battleEnemyStages = next}

data Side = Enemy | Player
  deriving stock (Eq)

stageFactor :: Context -> Side -> Text -> Double
stageFactor context side identifier = max 0 $ evaluate 1 formula [("stage", number stage)]
  where
    stages = (if side == Player then battlePlayerStages else battleEnemyStages) (contextBattle context)
    stage = Map.findWithDefault 0 identifier stages
    formula = (if identifier `elem` ["accuracy", "evasion"] then Rules.rulesAccuracyStageFormula else Rules.rulesStageMultiplierFormula) (rulesFor context)

stat :: Context -> Side -> Text -> Double
stat context side identifier = case active side context of
  Nothing -> 1
  Just critter -> max 1 (Map.findWithDefault 1 identifier (statsFor (contextBattle context) critter) * stageFactor context side identifier * statusMultiplier critter)
  where
    statusMultiplier critter = fromMaybe 1 $ do
      affliction <- Profile.profileStatus (Critter.battleProfile critter)
      definition <- Map.lookup (Affliction.afflictionStatus affliction) (Config.configStatuses (battleConfig (contextBattle context)))
      pure (Map.findWithDefault 1 identifier (Status.statusStatMultipliers definition))

statsFor :: Battle -> Critter -> Map Text Double
statsFor current critter = Map.map (\base -> max 1 (evaluate (number base) (Rules.rulesStatFormula rules) [("base", number base), ("level", number (Critter.level critter))])) (Species.speciesStats definition)
  where
    definition = Map.findWithDefault Species.defaultSpecies (Critter.species critter) (Config.configSpecies (battleConfig current))
    rules = Config.configRules (battleConfig current)

usable :: Critter -> Bool
usable = (> 0) . Critter.hitPoints
