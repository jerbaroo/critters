module Critters.Battle.Opponent (chooseAction, scoreMove) where

import Control.Monad.Trans.State.Strict (State)
import Control.Monad.Trans.State.Strict qualified as State
import Critters.Battle.Action (Action (..))
import Critters.Battle.Common (Context (..), Side (..))
import Critters.Battle.Common qualified as Common
import Critters.Battle.Damage qualified as Damage
import Critters.Number (number)
import Critters.State.Battle (Battle (..))
import Critters.State.BattleDefinition qualified as Definition
import Critters.State.BattleProfile qualified as Profile
import Critters.State.Critter qualified as Critter
import Critters.State.Move (Move)
import Critters.State.Move qualified as Move
import Critters.State.MoveEffect (MoveEffect (..), Target (..))
import Critters.State.MoveId (MoveId (..))
import Critters.State.Rules qualified as Rules
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)

chooseAction :: State Context Action
chooseAction = do
  context <- State.get
  case Common.availableMoves Enemy context of
    [] -> pure (UseMove (MoveId ""))
    first : rest -> case Definition.definitionOpponentPolicy (battleDefinition (contextBattle context)) of
      "random" -> maybe (UseMove (fst first)) (UseMove . fst) <$> Common.chooseWeighted [(entry, 1) | entry <- first : rest]
      "strongest" -> pure (UseMove (fst (foldl' (better context) first rest)))
      _ -> pure (UseMove (fst first))
  where
    better context old next = if scoreMove context (snd next) > scoreMove context (snd old) then next else old

scoreMove :: Context -> Move -> Double
scoreMove context move = Common.evaluate 0 (Rules.rulesAiScoreFormula rules)
  [ ("accuracy", max 0 (min 1 (Damage.accuracyFor context Enemy move))),
    ("damage", min targetHealth (damage + extraDamage)), ("heal", healing),
    ("hpRatio", health / max 1 maximumHealth), ("priority", number (Move.movePriority move)),
    ("status", utility), ("targetHP", targetHealth)
  ]
  where
    criticalChance = max 0 (min 1 (Common.evaluate 0 (Rules.rulesCriticalFormula rules) [("criticalRate", Move.moveCriticalRate move), ("speed", Common.stat context Enemy (Rules.rulesSpeedStat rules)), ("level", level Enemy), ("targetLevel", level Player)]))
    criticalMultiplier = max 0 (Common.evaluate 1 (Rules.rulesCriticalMultiplierFormula rules) [("level", level Enemy), ("targetLevel", level Player)])
    damage = number (Damage.damageFor context Enemy move (1 + criticalChance * (criticalMultiplier - 1)) ((Rules.rulesRandomDamageMin rules + Rules.rulesRandomDamageMax rules) / 2)) * hits
    (extraDamage, healing, utility) = foldl' add (0, 0, 0) (map effectValue (Move.moveEffects move))
    add (a, b, c) (x, y, z) = (a + x, b + y, c + z)
    effectValue = \case
      Chance probability effect -> let (a, b, c) = effectValue effect in (probability * a, probability * b, probability * c)
      ChangeHealth target formula ->
        let side = if target == Self then Enemy else Player
            hp = maybe 0 (number . Critter.hitPoints) (Common.active side context)
            maximumHP = maybe 1 (number . Critter.maxHitPoints) (Common.active side context)
            amount = Common.evaluate 0 formula [("damage", damage), ("hp", hp), ("maxHP", maximumHP), ("level", level Enemy), ("targetHP", hp), ("targetMaxHP", maximumHP)]
         in if target == Self then (0, max 0 (min (maximumHP - hp) amount), 0) else (max 0 (-amount), 0, 0)
      ChangeStage target identifier amount ->
        let stages = (if target == Self then battleEnemyStages else battlePlayerStages) (contextBattle context)
            old = Map.findWithDefault 0 identifier stages
            limit = Rules.rulesStageLimit rules
            delta = max (-limit) (min limit (old + max (-limit) (min limit amount))) - old
         in (0, 0, number (if target == Self then delta else -delta))
      ClearStatus target -> (0, 0, if maybe False (isJust . Profile.profileStatus . Critter.battleProfile) (Common.active (if target == Self then Enemy else Player) context) then (if target == Self then 1 else -1) else 0)
      InflictStatus target _ -> (0, 0, if maybe False (not . isJust . Profile.profileStatus . Critter.battleProfile) (Common.active (if target == Self then Enemy else Player) context) then 1 else 0)
    health = maybe 0 (number . Critter.hitPoints) (Common.active Enemy context)
    hits
      | Map.null (Move.moveHitWeights move) = (number (Move.moveMinHits move) + number (Move.moveMaxHits move)) / 2
      | otherwise = sum [number count * number weight | (count, weight) <- Map.toList (Move.moveHitWeights move)] / max 1 (sum (map number (Map.elems (Move.moveHitWeights move))))
    level participant = maybe 1 (number . Critter.level) (Common.active participant context)
    maximumHealth = maybe 1 (number . Critter.maxHitPoints) (Common.active Enemy context)
    rules = Common.rulesFor context
    targetHealth = maybe 0 (number . Critter.hitPoints) (Common.active Player context)
