module Critters.Battle.Opponent (chooseAction, scoreMove) where

import Control.Monad.Trans.State.Strict (State)
import Control.Monad.Trans.State.Strict qualified as State
import Critters.Battle.Action (Action (..))
import Critters.Battle.Common (Context (..), Side (..))
import Critters.Battle.Common qualified as Common
import Critters.Battle.Damage qualified as Damage
import Critters.Number (number)
import Critters.Stage qualified as Stage
import Critters.State.Battle (Battle (..))
import Critters.State.BattleDefinition qualified as Definition
import Critters.State.BattleProfile qualified as Profile
import Critters.State.CombatConfig qualified as Config
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
        first : rest -> case context.battle.definition.opponentPolicy of
            "random" -> maybe (UseMove (fst first)) (UseMove . fst) <$> Common.chooseWeighted [(entry, 1) | entry <- first : rest]
            "strongest" -> pure (UseMove (fst (foldl' (better context) first rest)))
            _ -> pure (UseMove (fst first))
  where
    better context old next = if scoreMove context (snd next) > scoreMove context (snd old) then next else old

scoreMove :: Context -> Move -> Double
scoreMove context move =
    Common.evaluate
        rules.aiScoreFormula
        [ ("accuracy", max 0 (min 1 (Damage.accuracyFor context Enemy move)))
        , ("damage", min targetHealth (damage + extraDamage))
        , ("heal", healing)
        , ("hpRatio", health / max 1 maximumHealth)
        , ("priority", number move.priority)
        , ("status", utility)
        , ("targetHP", targetHealth)
        ]
  where
    criticalChance = max 0 (min 1 (Common.evaluate rules.criticalFormula [("criticalRate", move.criticalRate), ("speed", Common.stat context Enemy rules.speedStat), ("level", level Enemy), ("targetLevel", level Player)]))
    criticalMultiplier = max 0 (Common.evaluate rules.criticalMultiplierFormula [("level", level Enemy), ("targetLevel", level Player)])
    damage = number (Damage.damageFor context Enemy move (1 + criticalChance * (criticalMultiplier - 1)) ((rules.randomDamageMin + rules.randomDamageMax) / 2)) * hits
    (extraDamage, healing, utility) = foldl' add (0, 0, 0) (map effectValue move.effects)
    add (a, b, c) (x, y, z) = (a + x, b + y, c + z)
    effectValue = \case
        Chance probability effect -> let (a, b, c) = effectValue effect in (probability * a, probability * b, probability * c)
        ChangeHealth target formula ->
            let side = if target == Self then Enemy else Player
                hp = maybe 0 (number . (.hitPoints)) (Common.active side context)
                maximumHP = maybe 1 (number . (.maxHitPoints)) (Common.active side context)
                amount = Common.evaluate formula [("damage", damage), ("hp", hp), ("maxHP", maximumHP), ("level", level Enemy), ("targetHP", hp), ("targetMaxHP", maximumHP)]
             in if target == Self then (0, max 0 (min (maximumHP - hp) amount), 0) else (max 0 (-amount), 0, 0)
        ChangeStage target identifier amount ->
            let stages = (if target == Self then (.enemyStages) else (.playerStages)) context.battle
                (_, change) = Stage.changeStage context.battle.config.stages stages identifier amount
                delta = change.after - change.before
             in (0, 0, number (if target == Self then delta else -delta))
        ClearStatus target -> (0, 0, if maybe False (isJust . (.battleProfile.status)) (Common.active (if target == Self then Enemy else Player) context) then (if target == Self then 1 else -1) else 0)
        InflictStatus target _ -> (0, 0, if maybe False (not . isJust . (.battleProfile.status)) (Common.active (if target == Self then Enemy else Player) context) then 1 else 0)
    health = maybe 0 (number . (.hitPoints)) (Common.active Enemy context)
    hits
        | Map.null move.hitWeights = (number move.minHits + number move.maxHits) / 2
        | otherwise = sum [number count * number weight | (count, weight) <- Map.toList move.hitWeights] / max 1 (sum (map number (Map.elems move.hitWeights)))
    level participant = maybe 1 (number . (.level)) (Common.active participant context)
    maximumHealth = maybe 1 (number . (.maxHitPoints)) (Common.active Enemy context)
    rules = Common.rulesFor context
    targetHealth = maybe 0 (number . (.hitPoints)) (Common.active Player context)
