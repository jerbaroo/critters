module Critters.State.Rules (defaultRules, Rules (..)) where

import Autodocodec qualified as Codec
import Autodocodec (Autodocodec (..), HasCodec)
import Critters.Codec qualified as NumberCodec
import Critters.Formula qualified as Formula
import Critters.Formula (Formula (..))
import Critters.State.Move (Move, defaultMove)
import Critters.State.Move qualified as Move
import Data.Aeson qualified as Aeson
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)

defaultRules :: Rules
defaultRules = Rules
  { rulesAccuracyFormula = Formula "accuracy * accuracyStage / evasionStage",
    rulesAccuracyPerHit = False,
    rulesAccuracyStageFormula = Formula "if(stage >= 0, (3 + stage) / 3, 3 / (3 - stage))",
    rulesAffinityChart = Map.empty,
    rulesAiScoreFormula = Formula "damage * accuracy + heal * if(hpRatio < 0.4, 2, 0.2) + status * 2",
    rulesCaptureFormula = Formula "rate * itemBonus * (1 - hp / (2 * maxHP)) * statusBonus",
    rulesCaptureGivesExperience = False,
    rulesCriticalFormula = Formula "criticalRate",
    rulesCriticalIgnoresStages = False,
    rulesCriticalMultiplierFormula = Formula "2",
    rulesCriticalPerHit = True,
    rulesDamageFormula = Formula "max(1, floor(power * attack / defense / 5 * critical * affinity * sameAffinity * variance))",
    rulesDefeatRecovery = 1,
    rulesEscapeFormula = Formula "speed / max(1, enemySpeed) / 2 + attempts / 4",
    rulesEscapePriority = 8,
    rulesExperienceFormula = Formula "level ^ 3",
    rulesExperienceRewardFormula = Formula "floor(yield * level / max(1, participants))",
    rulesExperienceSharing = "participants",
    rulesFallbackMove = defaultMove,
    rulesHealthFormula = Formula "base + level * 2",
    rulesItemPriority = 6,
    rulesLearningPolicy = "keep-newest",
    rulesMaxKnownMoves = 4,
    rulesMaxLevel = 100,
    rulesMinimumDamage = 1,
    rulesRandomDamageMax = 1,
    rulesRandomDamageMin = 1,
    rulesSameAffinityBonus = 1.5,
    rulesSpeedStat = "speed",
    rulesStageLimit = 6,
    rulesStageMultiplierFormula = Formula "if(stage >= 0, (2 + stage) / 2, 2 / (2 - stage))",
    rulesStatFormula = Formula "max(1, floor(base * (1 + level / 50)))",
    rulesStatusCaptureBonuses = Map.empty,
    rulesSwitchPriority = 7,
    rulesTieBreak = "player"
  }

data Rules = Rules
  { rulesAccuracyFormula :: Formula,
    rulesAccuracyPerHit :: Bool,
    rulesAccuracyStageFormula :: Formula,
    rulesAffinityChart :: Map Text (Map Text Double),
    rulesAiScoreFormula :: Formula,
    rulesCaptureFormula :: Formula,
    rulesCaptureGivesExperience :: Bool,
    rulesCriticalFormula :: Formula,
    rulesCriticalIgnoresStages :: Bool,
    rulesCriticalMultiplierFormula :: Formula,
    rulesCriticalPerHit :: Bool,
    rulesDamageFormula :: Formula,
    rulesDefeatRecovery :: Double,
    rulesEscapeFormula :: Formula,
    rulesEscapePriority :: Int,
    rulesExperienceFormula :: Formula,
    rulesExperienceRewardFormula :: Formula,
    rulesExperienceSharing :: Text,
    rulesFallbackMove :: Move,
    rulesHealthFormula :: Formula,
    rulesItemPriority :: Int,
    rulesLearningPolicy :: Text,
    rulesMaxKnownMoves :: Int,
    rulesMaxLevel :: Int,
    rulesMinimumDamage :: Int,
    rulesRandomDamageMax :: Double,
    rulesRandomDamageMin :: Double,
    rulesSameAffinityBonus :: Double,
    rulesSpeedStat :: Text,
    rulesStageLimit :: Int,
    rulesStageMultiplierFormula :: Formula,
    rulesStatFormula :: Formula,
    rulesStatusCaptureBonuses :: Map Text Double,
    rulesSwitchPriority :: Int,
    rulesTieBreak :: Text
  }
  deriving stock (Eq)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec Rules)

instance HasCodec Rules where
  codec = Codec.bimapCodec validate id $ Codec.object "Rules" $ Rules
    <$> Codec.optionalFieldWithOmittedDefault "accuracyFormula" (Formula "accuracy * accuracyStage / evasionStage") "Hit probability; the result is clamped to zero through one." Codec..= rulesAccuracyFormula
    <*> Codec.optionalFieldWithOmittedDefault "accuracyPerHit" False "Roll accuracy for each hit rather than once per move." Codec..= rulesAccuracyPerHit
    <*> Codec.optionalFieldWithOmittedDefault "accuracyStageFormula" (Formula "if(stage >= 0, (3 + stage) / 3, 3 / (3 - stage))") "Accuracy and evasion stage multiplier." Codec..= rulesAccuracyStageFormula
    <*> Codec.optionalFieldWithOmittedDefaultWith "affinityChart" (Codec.mapCodec (Codec.mapCodec NumberCodec.finiteDoubleCodec)) (Map.empty) "Attack affinity to defending affinity multipliers." Codec..= rulesAffinityChart
    <*> Codec.optionalFieldWithOmittedDefault "aiScoreFormula" (Formula "damage * accuracy + heal * if(hpRatio < 0.4, 2, 0.2) + status * 2") "Score used by the strongest opponent policy." Codec..= rulesAiScoreFormula
    <*> Codec.optionalFieldWithOmittedDefault "captureFormula" (Formula "rate * itemBonus * (1 - hp / (2 * maxHP)) * statusBonus") "Capture probability; the result is clamped to zero through one." Codec..= rulesCaptureFormula
    <*> Codec.optionalFieldWithOmittedDefault "captureGivesExperience" (False) "Whether a successful capture grants experience." Codec..= rulesCaptureGivesExperience
    <*> Codec.optionalFieldWithOmittedDefault "criticalFormula" (Formula "criticalRate") "Critical probability." Codec..= rulesCriticalFormula
    <*> Codec.optionalFieldWithOmittedDefault "criticalIgnoresStages" (False) "Ignore attack and defense stages on critical hits." Codec..= rulesCriticalIgnoresStages
    <*> Codec.optionalFieldWithOmittedDefault "criticalMultiplierFormula" (Formula "2") "Critical damage multiplier." Codec..= rulesCriticalMultiplierFormula
    <*> Codec.optionalFieldWithOmittedDefault "criticalPerHit" True "Roll critical chance independently for each hit." Codec..= rulesCriticalPerHit
    <*> Codec.optionalFieldWithOmittedDefault "damageFormula" (Formula "max(1, floor(power * attack / defense / 5 * critical * affinity * sameAffinity * variance))") "Damage expression; affinity immunity still prevents damage." Codec..= rulesDamageFormula
    <*> Codec.optionalFieldWithOmittedDefaultWith "defeatRecovery" NumberCodec.probabilityCodec (1) "Fraction of maximum health restored after defeat." Codec..= rulesDefeatRecovery
    <*> Codec.optionalFieldWithOmittedDefault "escapeFormula" (Formula "speed / max(1, enemySpeed) / 2 + attempts / 4") "Escape probability." Codec..= rulesEscapeFormula
    <*> Codec.optionalFieldWithOmittedDefault "escapePriority" (8) "Action priority for escape attempts." Codec..= rulesEscapePriority
    <*> Codec.optionalFieldWithOmittedDefault "experienceFormula" (Formula "level ^ 3") "Total experience threshold for a level." Codec..= rulesExperienceFormula
    <*> Codec.optionalFieldWithOmittedDefault "experienceRewardFormula" (Formula "floor(yield * level / max(1, participants))") "Experience per eligible party member." Codec..= rulesExperienceRewardFormula
    <*> Codec.optionalFieldWithOmittedDefault "experienceSharing" ("participants") "Experience recipients: participants or party." Codec..= rulesExperienceSharing
    <*> Codec.optionalFieldWithOmittedDefault "fallbackMove" (defaultMove) "Action used when no learned move has remaining uses." Codec..= rulesFallbackMove
    <*> Codec.optionalFieldWithOmittedDefault "healthFormula" (Formula "base + level * 2") "Maximum health on level changes and generated encounters." Codec..= rulesHealthFormula
    <*> Codec.optionalFieldWithOmittedDefault "itemPriority" (6) "Action priority for items." Codec..= rulesItemPriority
    <*> Codec.optionalFieldWithOmittedDefault "learningPolicy" ("keep-newest") "Move overflow policy: keep-newest or keep-oldest." Codec..= rulesLearningPolicy
    <*> Codec.optionalFieldWithOmittedDefault "maxKnownMoves" (4) "Game-wide learned move limit." Codec..= rulesMaxKnownMoves
    <*> Codec.optionalFieldWithOmittedDefault "maxLevel" (100) "Game-wide level limit." Codec..= rulesMaxLevel
    <*> Codec.optionalFieldWithOmittedDefault "minimumDamage" (1) "Minimum damage of a nonimmune damaging hit." Codec..= rulesMinimumDamage
    <*> Codec.optionalFieldWithOmittedDefaultWith "randomDamageMax" NumberCodec.finiteDoubleCodec (1) "Upper damage variance multiplier." Codec..= rulesRandomDamageMax
    <*> Codec.optionalFieldWithOmittedDefaultWith "randomDamageMin" NumberCodec.finiteDoubleCodec (1) "Lower damage variance multiplier." Codec..= rulesRandomDamageMin
    <*> Codec.optionalFieldWithOmittedDefaultWith "sameAffinityBonus" NumberCodec.finiteDoubleCodec (1.5) "Multiplier when a move matches an attacking affinity." Codec..= rulesSameAffinityBonus
    <*> Codec.optionalFieldWithOmittedDefault "speedStat" ("speed") "Stat used to order actions." Codec..= rulesSpeedStat
    <*> Codec.optionalFieldWithOmittedDefault "stageLimit" (6) "Absolute limit on temporary stat stages." Codec..= rulesStageLimit
    <*> Codec.optionalFieldWithOmittedDefault "stageMultiplierFormula" (Formula "if(stage >= 0, (2 + stage) / 2, 2 / (2 - stage))") "Temporary stat multiplier." Codec..= rulesStageMultiplierFormula
    <*> Codec.optionalFieldWithOmittedDefault "statFormula" (Formula "max(1, floor(base * (1 + level / 50)))") "Combat stat calculated from its base value and level." Codec..= rulesStatFormula
    <*> Codec.optionalFieldWithOmittedDefaultWith "statusCaptureBonuses" (Codec.mapCodec NumberCodec.finiteDoubleCodec) (Map.empty) "Capture multipliers keyed by status." Codec..= rulesStatusCaptureBonuses
    <*> Codec.optionalFieldWithOmittedDefault "switchPriority" (7) "Action priority for a voluntary switch." Codec..= rulesSwitchPriority
    <*> Codec.optionalFieldWithOmittedDefault "tieBreak" ("player") "Equal-speed order: player, opponent or random." Codec..= rulesTieBreak
    where
      validate value
        | rulesMaxKnownMoves value < 1 || rulesMaxLevel value < 1 || rulesMaxLevel value > 10000 = Left "Move and level limits must be positive; maxLevel must not exceed 10000."
        | Move.moveMaxUses (rulesFallbackMove value) /= 0 = Left "The fallback move must have unlimited uses."
        | rulesStageLimit value < 0 || rulesStageLimit value > 100 || rulesMinimumDamage value < 0 = Left "Stage limit must be between zero and 100 and minimum damage must be nonnegative."
        | rulesRandomDamageMin value < 0 || rulesRandomDamageMax value < rulesRandomDamageMin value || rulesSameAffinityBonus value < 0 = Left "Damage multipliers must be nonnegative and ordered."
        | any (< 0) (concatMap Map.elems (Map.elems (rulesAffinityChart value))) || any (< 0) (Map.elems (rulesStatusCaptureBonuses value)) = Left "Affinity and capture multipliers must be nonnegative."
        | rulesTieBreak value `notElem` ["player", "opponent", "random"] || rulesExperienceSharing value `notElem` ["participants", "party"] || rulesLearningPolicy value `notElem` ["keep-newest", "keep-oldest"] = Left "Unknown battle policy."
        | either (const True) (any (`notElem` ["accuracy", "accuracyStage", "evasionStage", "level", "targetLevel"])) (Formula.variables (rulesAccuracyFormula value)) = Left "Unknown variable in accuracyFormula."
        | either (const True) (any (`notElem` ["stage"])) (Formula.variables (rulesAccuracyStageFormula value)) = Left "Unknown variable in accuracyStageFormula."
        | either (const True) (any (`notElem` ["damage", "accuracy", "heal", "hpRatio", "status", "priority", "targetHP"])) (Formula.variables (rulesAiScoreFormula value)) = Left "Unknown variable in aiScoreFormula."
        | either (const True) (any (`notElem` ["rate", "itemBonus", "hp", "maxHP", "statusBonus", "level", "targetLevel"])) (Formula.variables (rulesCaptureFormula value)) = Left "Unknown variable in captureFormula."
        | either (const True) (any (`notElem` ["criticalRate", "speed", "level", "targetLevel"])) (Formula.variables (rulesCriticalFormula value)) = Left "Unknown variable in criticalFormula."
        | either (const True) (any (`notElem` ["level", "targetLevel"])) (Formula.variables (rulesCriticalMultiplierFormula value)) = Left "Unknown variable in criticalMultiplierFormula."
        | either (const True) (any (`notElem` ["power", "attack", "defense", "baseAttack", "baseDefense", "critical", "affinity", "sameAffinity", "variance", "level", "targetLevel"])) (Formula.variables (rulesDamageFormula value)) = Left "Unknown variable in damageFormula."
        | either (const True) (any (`notElem` ["speed", "enemySpeed", "attempts", "level", "enemyLevel"])) (Formula.variables (rulesEscapeFormula value)) = Left "Unknown variable in escapeFormula."
        | either (const True) (any (`notElem` ["level"])) (Formula.variables (rulesExperienceFormula value)) = Left "Unknown variable in experienceFormula."
        | either (const True) (any (`notElem` ["yield", "level", "participants"])) (Formula.variables (rulesExperienceRewardFormula value)) = Left "Unknown variable in experienceRewardFormula."
        | either (const True) (any (`notElem` ["base", "level"])) (Formula.variables (rulesHealthFormula value)) = Left "Unknown variable in healthFormula."
        | either (const True) (any (`notElem` ["stage"])) (Formula.variables (rulesStageMultiplierFormula value)) = Left "Unknown variable in stageMultiplierFormula."
        | either (const True) (any (`notElem` ["base", "level"])) (Formula.variables (rulesStatFormula value)) = Left "Unknown variable in statFormula."
        | otherwise = Right value
