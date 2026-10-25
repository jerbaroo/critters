module Critters.Battle.Damage (accuracyFor, damageFor) where

import Critters.Battle.Common (Context (..), Side)
import Critters.Battle.Common qualified as Common
import Critters.Number (integer, number)
import Critters.State.Battle (Battle (..))
import Critters.State.CombatConfig qualified as Config
import Critters.State.Critter qualified as Critter
import Critters.State.Move (Move)
import Critters.State.Move qualified as Move
import Critters.State.Rules qualified as Rules
import Critters.State.Species qualified as Species
import Data.Map.Strict qualified as Map

accuracyFor :: Context -> Side -> Move -> Double
accuracyFor context side move =
    Common.evaluate
        ((Common.rulesFor context).accuracyFormula)
        [ ("accuracy", move.accuracy)
        , ("accuracyStage", Common.stageFactor context side "accuracy")
        , ("evasionStage", Common.stageFactor context (Common.opposite side) "evasion")
        , ("level", maybe 1 (number . (.level)) (Common.active side context))
        , ("targetLevel", maybe 1 (number . (.level)) (Common.active (Common.opposite side) context))
        ]

damageFor :: Context -> Side -> Move -> Double -> Double -> Int
damageFor context side move critical variance
    | move.power <= 0 || typeMultiplier == 0 = 0
    | otherwise =
        max rules.minimumDamage $
            integer $
                Common.evaluate
                    rules.damageFormula
                    [ ("affinity", typeMultiplier)
                    , ("attack", attack)
                    , ("baseAttack", baseAttack)
                    , ("baseDefense", baseDefense)
                    , ("critical", critical)
                    , ("defense", defense)
                    , ("level", level side)
                    , ("power", number move.power)
                    , ("sameAffinity", sameAffinity)
                    , ("targetLevel", level (Common.opposite side))
                    , ("variance", variance)
                    ]
  where
    typeMultiplier = product [Map.findWithDefault 1 defending (Map.findWithDefault Map.empty move.affinity rules.typeChart) | defending <- typesFor (Common.opposite side)]
    typesFor participant = maybe [] (maybe [] (.types) . (\critter -> Map.lookup critter.species context.battle.config.species)) (Common.active participant context)
    attack = if critical > 1 && rules.criticalIgnoresStages then baseAttack else Common.stat context side move.attackStat
    baseAttack = Common.baseStat context side move.attackStat
    baseDefense = Common.baseStat context (Common.opposite side) move.defenseStat
    defense = if critical > 1 && rules.criticalIgnoresStages then baseDefense else Common.stat context (Common.opposite side) move.defenseStat
    level participant = maybe 1 (number . (.level)) (Common.active participant context)
    rules = Common.rulesFor context
    sameAffinity = if move.affinity `elem` typesFor side then rules.sameAffinityBonus else 1
