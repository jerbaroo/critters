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
    0
    (Rules.rulesAccuracyFormula (Common.rulesFor context))
    [ ("accuracy", Move.moveAccuracy move),
      ("accuracyStage", Common.stageFactor context side "accuracy"),
      ("evasionStage", Common.stageFactor context (Common.opposite side) "evasion"),
      ("level", maybe 1 (number . Critter.level) (Common.active side context)),
      ("targetLevel", maybe 1 (number . Critter.level) (Common.active (Common.opposite side) context))
    ]

damageFor :: Context -> Side -> Move -> Double -> Double -> Int
damageFor context side move critical variance
  | Move.movePower move <= 0 || affinity == 0 = 0
  | otherwise =
      max (Rules.rulesMinimumDamage rules)
        $ integer
        $ Common.evaluate
          0
          (Rules.rulesDamageFormula rules)
          [ ("affinity", affinity),
            ("attack", attack),
            ("baseAttack", baseAttack),
            ("baseDefense", baseDefense),
            ("critical", critical),
            ("defense", defense),
            ("level", level side),
            ("power", number (Move.movePower move)),
            ("sameAffinity", sameAffinity),
            ("targetLevel", level (Common.opposite side)),
            ("variance", variance)
          ]
  where
    affinity = product [Map.findWithDefault 1 defending (Map.findWithDefault Map.empty (Move.moveAffinity move) (Rules.rulesAffinityChart rules)) | defending <- affinities (Common.opposite side)]
    affinities participant = maybe [] (maybe [] Species.speciesAffinities . (\critter -> Map.lookup (Critter.species critter) (Config.configSpecies (battleConfig (contextBattle context))))) (Common.active participant context)
    attack = if critical > 1 && Rules.rulesCriticalIgnoresStages rules then baseAttack else Common.stat context side (Move.moveAttackStat move)
    baseAttack = Common.baseStat context side (Move.moveAttackStat move)
    baseDefense = Common.baseStat context (Common.opposite side) (Move.moveDefenseStat move)
    defense = if critical > 1 && Rules.rulesCriticalIgnoresStages rules then baseDefense else Common.stat context (Common.opposite side) (Move.moveDefenseStat move)
    level participant = maybe 1 (number . Critter.level) (Common.active participant context)
    rules = Common.rulesFor context
    sameAffinity = if Move.moveAffinity move `elem` affinities side then Rules.rulesSameAffinityBonus rules else 1
