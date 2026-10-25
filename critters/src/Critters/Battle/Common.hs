-- | Pure battle operations shared by turn resolution, menus and encounters.
module Critters.Battle.Common (
    active,
    activeIndex,
    allCritters,
    availableMoves,
    baseStat,
    changedEvents,
    chance,
    chooseInt,
    chooseWeighted,
    Context (..),
    contextFor,
    critterName,
    displayName,
    emit,
    emitDamage,
    emitKey,
    endBattle,
    evaluate,
    healParty,
    knownMoves,
    modifyActive,
    modifyAt,
    moveRemaining,
    opposite,
    randomUnit,
    rulesFor,
    setCritter,
    setStage,
    Side (..),
    stageFactor,
    stat,
    statsFor,
    usable,
    validCritter,
)
where

import Control.Monad.Trans.State.Strict (State)
import Control.Monad.Trans.State.Strict qualified as State
import Critters.Event (Event)
import Critters.Event qualified as Event
import Critters.Expression (Expression)
import Critters.Expression qualified as Expression
import Critters.Message qualified as Message
import Critters.Number (integer, number)
import Critters.Stage qualified as Stage
import Critters.State.Affliction qualified as Affliction
import Critters.State.Battle (Battle (..))
import Critters.State.Battle qualified as Battle
import Critters.State.BattleDefinition qualified as Definition
import Critters.State.BattleOutcome (BattleOutcome (..))
import Critters.State.BattlePhase (BattlePhase (Finished))
import Critters.State.BattleProfile qualified as Profile
import Critters.State.BattleUpdate (BattleUpdate (BattleUpdate))
import Critters.State.CombatConfig qualified as Config
import Critters.State.Critter (Critter)
import Critters.State.Critter qualified as Critter
import Critters.State.GameState (GameState)
import Critters.State.GameState qualified as GameState
import Critters.State.Message (Argument, BattleMessage (..), Message, MessageKey, Presentation (..))
import Critters.State.Move (Move)
import Critters.State.Move qualified as Move
import Critters.State.MoveId (MoveId (..))
import Critters.State.Party (Party (..))
import Critters.State.Rules (Rules)
import Critters.State.Rules qualified as Rules
import Critters.State.Species qualified as Species
import Critters.State.StatName (StatName)
import Critters.State.StatusDefinition qualified as Status
import Data.List (nub)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Witch (from, tryFrom)

active :: Side -> Context -> Maybe Critter
active side context = case drop (activeIndex side context) (allCritters side context) of
    value : _ | activeIndex side context >= 0 -> Just value
    _ -> Nothing

activeIndex :: Side -> Context -> Int
activeIndex side context = (if side == Player then (.activePlayer) else (.activeOpponent)) context.battle

allCritters :: Side -> Context -> [Critter]
allCritters Player = (.party.members)
allCritters Enemy = (.battle.opponentParty)

availableMoves :: Side -> Context -> [(MoveId, Move)]
availableMoves side context = case active side context of
    Nothing -> []
    Just critter -> case mapMaybe (available critter) (knownMoves context.battle critter) of
        [] -> [(MoveId "", (rulesFor context).fallbackMove)]
        moves -> moves
  where
    available critter identifier = do
        definition <- Map.lookup identifier context.battle.config.moves
        if definition.maxUses == 0 || moveRemaining critter identifier definition > 0 then Just (identifier, definition) else Nothing

baseStat :: Context -> Side -> StatName -> Double
baseStat context side identifier = case active side context of
    Nothing -> 1
    Just critter -> max 1 (Map.findWithDefault 1 identifier (statsFor context.battle critter) * statusMultiplier critter)
  where
    statusMultiplier critter = fromMaybe 1 $ do
        affliction <- critter.battleProfile.status
        definition <- Map.lookup affliction.status context.battle.config.statuses
        pure (Map.findWithDefault 1 identifier definition.statMultipliers)

changedEvents :: GameState -> Context -> [Event]
changedEvents starting context
    | starting.battle == Just context.battle && starting.party == context.party && starting.inventory == context.inventory && starting.randomSeed == context.seed = []
    | otherwise = [Event.BattleChanged (BattleUpdate context.battle (changed starting.inventory context.inventory) (changed starting.party context.party) (changed starting.randomSeed context.seed))]
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

chooseWeighted :: [(a, Int)] -> State Context (Maybe a)
chooseWeighted entries = case filter ((> 0) . snd) entries of
    [] -> pure Nothing
    [(value, _)] -> pure (Just value)
    values -> do
        roll <- randomUnit
        pure (pick (roll * sum (map (number . snd) values)) values)
  where
    pick _ [] = Nothing
    pick value ((entry, weight) : rest)
        | value < number weight || null rest = Just entry
        | otherwise = pick (value - number weight) rest

data Context = Context
    { battle :: Battle
    , inventory :: Map Text Int
    , limit :: Int
    , party :: Party
    , seed :: Int
    }
    deriving stock (Eq)

contextFor :: GameState -> Maybe Context
contextFor state = (\activeBattle -> Context activeBattle state.inventory state.maxPartySize state.party state.randomSeed) <$> state.battle

critterName :: Critter -> Argument
critterName critter = Message.reference ("{combat.species." <> critter.species <> ".name}")

displayName :: Context -> Critter -> Text
displayName context critter = case Map.lookup critter.species context.battle.config.species of
    Just definition | not (Text.null definition.name) -> definition.name
    _ -> critter.species

emit :: Message -> State Context ()
emit message = State.modify $ \context -> context{battle = context.battle{Battle.messages = context.battle.messages <> [BattleMessage message Dialogue]}}

emitDamage :: Message -> State Context ()
emitDamage message = State.modify $ \context -> context{battle = context.battle{Battle.messages = context.battle.messages <> [BattleMessage message HealthChange]}}

emitKey :: MessageKey -> [(Text, Argument)] -> State Context ()
emitKey key = emit . Message.named key

endBattle :: BattleOutcome -> State Context ()
endBattle outcome = do
    State.modify $ \context ->
        context
            { battle = context.battle{Battle.outcome = Just outcome, Battle.phase = Finished, Battle.selected = 0}
            , party = if outcome == Defeat then healParty ((rulesFor context).defeatRecovery) context.party else context.party
            }
    context <- State.get
    emitKey (case outcome of Captured -> "battleCaptureComplete"; Defeat -> "battleDefeat"; Escaped -> "battleEscaped"; Victory -> "battleVictory") [("opponent", Message.reference context.battle.definition.name)]

evaluate :: Expression -> [(Text, Double)] -> Double
evaluate formula environment = either (error . from) id (Expression.evaluateNumber (Map.fromList environment) formula)

healParty :: Double -> Party -> Party
healParty fraction (Party critters)
    | fraction <= 0 = Party critters
    | otherwise = Party (map recover critters)
  where
    recover critter =
        critter
            { Critter.hitPoints = max critter.hitPoints (max 1 (min critter.maxHitPoints (integer (number critter.maxHitPoints * fraction))))
            , Critter.battleProfile = critter.battleProfile{Profile.remainingUses = Map.empty, Profile.status = Nothing}
            }

knownMoves :: Battle -> Critter -> [MoveId]
knownMoves current critter = limit $ nub $ case critter.battleProfile.knownMoves of
    [] -> maybe [] (concatMap snd . filter ((<= critter.level) . fst) . Map.toAscList . (.learnset)) (Map.lookup critter.species current.config.species)
    moves -> moves
  where
    rules = current.config.rules
    limit moves
        | rules.learningPolicy == "keep-oldest" = take rules.maxKnownMoves moves
        | otherwise = reverse (take rules.maxKnownMoves (reverse moves))

modifyActive :: Side -> (Critter -> Critter) -> State Context ()
modifyActive side change = State.modify $ \context -> maybe context (\critter -> setCritter side (activeIndex side context) (change critter) context) (active side context)

modifyAt :: Int -> (a -> a) -> [a] -> [a]
modifyAt index change = zipWith (\current value -> if current == index then change value else value) [0 ..]

moveRemaining :: Critter -> MoveId -> Move -> Int
moveRemaining critter identifier definition = Map.findWithDefault definition.maxUses identifier critter.battleProfile.remainingUses

opposite :: Side -> Side
opposite Enemy = Player
opposite Player = Enemy

randomUnit :: State Context Double
randomUnit = do
    context <- State.get
    let next = (1103515245 * from @Int @Integer context.seed + 12345) `mod` 2147483648
        seed = either (const 1) id (tryFrom @Integer @Int next)
    State.put context{seed = seed}
    pure (number seed / 2147483648)

rulesFor :: Context -> Rules
rulesFor = (.battle.config.rules)

setCritter :: Side -> Int -> Critter -> Context -> Context
setCritter Enemy index critter context = context{battle = context.battle{Battle.opponentParty = modifyAt index (const critter) context.battle.opponentParty}}
setCritter Player index critter context = context{party = Party (modifyAt index (const critter) context.party.members)}

setStage :: Side -> StatName -> Int -> Context -> (Stage.StageChange, Context)
setStage side identifier delta context = (change, context{battle = updated})
  where
    current = context.battle
    stages = (if side == Player then (.playerStages) else (.enemyStages)) current
    (next, change) = Stage.changeStage current.config.stages stages identifier delta
    updated = if side == Player then current{Battle.playerStages = next} else current{Battle.enemyStages = next}

data Side = Enemy | Player
    deriving stock (Eq)

stageFactor :: Context -> Side -> StatName -> Double
stageFactor context side identifier = Stage.stageFactor context.battle.config.stages stages identifier
  where
    stages = (if side == Player then (.playerStages) else (.enemyStages)) context.battle

stat :: Context -> Side -> StatName -> Double
stat context side identifier = max 1 (baseStat context side identifier * stageFactor context side identifier)

statsFor :: Battle -> Critter -> Map StatName Double
statsFor current critter = Map.map (\base -> max 1 (evaluate rules.statFormula [("base", number base), ("level", number critter.level)])) definition.stats.values
  where
    definition = Map.findWithDefault Species.defaultSpecies critter.species current.config.species
    rules = current.config.rules

usable :: Critter -> Bool
usable = (> 0) . (.hitPoints)

validCritter :: Config.CombatConfig -> Critter -> Bool
validCritter config critter =
    Map.member critter.species config.species
        && critter.level <= config.rules.maxLevel
        && all (`Map.member` config.moves) profile.knownMoves
        && all validUses (Map.toList profile.remainingUses)
        && maybe True (\affliction -> Map.member affliction.status config.statuses) profile.status
  where
    profile = critter.battleProfile
    validUses (identifier, remaining) = case Map.lookup identifier config.moves of
        Just move -> remaining >= 0 && (move.maxUses == 0 || remaining <= move.maxUses)
        Nothing -> False
