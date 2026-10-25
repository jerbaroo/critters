module Critters.State.Rules (defaultRules, Rules (..)) where

import Autodocodec (Autodocodec (..), HasCodec)
import Autodocodec qualified as Codec
import Critters.Codec qualified as NumberCodec
import Critters.Formula (Formula (..))
import Critters.Formula qualified as Formula
import Critters.Number (integer, number)
import Critters.State.Move (Move, defaultMove)
import Critters.State.Move qualified as Move
import Data.Aeson qualified as Aeson
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)

defaultRules :: Rules
defaultRules =
    Rules
        { accuracyFormula = Formula "accuracy * accuracyStage / evasionStage"
        , accuracyPerHit = False
        , accuracyStageFormula = Formula "if(stage >= 0, (3 + stage) / 3, 3 / (3 - stage))"
        , aiScoreFormula = Formula "damage * accuracy + heal * if(hpRatio < 0.4, 2, 0.2) + status * 2"
        , captureFormula = Formula "rate * itemBonus * (1 - hp / (2 * maxHP)) * statusBonus"
        , captureGivesExperience = False
        , criticalFormula = Formula "criticalRate"
        , criticalIgnoresStages = False
        , criticalMultiplierFormula = Formula "2"
        , criticalPerHit = True
        , damageFormula = Formula "max(1, floor(power * attack / defense / 5 * critical * affinity * sameAffinity * variance))"
        , defeatRecovery = 1
        , encountersAfterWarp = False
        , escapeFormula = Formula "speed / max(1, enemySpeed) / 2 + attempts / 4"
        , escapePriority = 8
        , experienceForFainted = False
        , experienceFormula = Formula "level ^ 3"
        , experienceRewardFormula = Formula "floor(yield * level / max(1, participants))"
        , experienceSharing = "participants"
        , fallbackMove = defaultMove
        , healthFormula = Formula "base + level * 2"
        , itemPriority = 6
        , learningPolicy = "keep-newest"
        , maxKnownMoves = 4
        , maxLevel = 100
        , minimumDamage = 1
        , randomDamageMax = 1
        , randomDamageMin = 1
        , resetStagesOnSwitch = True
        , sameAffinityBonus = 1.5
        , simultaneousDefeatWins = False
        , speedStat = "speed"
        , stageLimit = 6
        , stageMultiplierFormula = Formula "if(stage >= 0, (2 + stage) / 2, 2 / (2 - stage))"
        , statFormula = Formula "max(1, floor(base * (1 + level / 50)))"
        , statusCaptureBonuses = Map.empty
        , switchPriority = 7
        , tieBreak = "player"
        , typeChart = Map.empty
        }

data Rules = Rules
    { accuracyFormula :: Formula
    , accuracyPerHit :: Bool
    , accuracyStageFormula :: Formula
    , aiScoreFormula :: Formula
    , captureFormula :: Formula
    , captureGivesExperience :: Bool
    , criticalFormula :: Formula
    , criticalIgnoresStages :: Bool
    , criticalMultiplierFormula :: Formula
    , criticalPerHit :: Bool
    , damageFormula :: Formula
    , defeatRecovery :: Double
    , encountersAfterWarp :: Bool
    , escapeFormula :: Formula
    , escapePriority :: Int
    , experienceForFainted :: Bool
    , experienceFormula :: Formula
    , experienceRewardFormula :: Formula
    , experienceSharing :: Text
    , fallbackMove :: Move
    , healthFormula :: Formula
    , itemPriority :: Int
    , learningPolicy :: Text
    , maxKnownMoves :: Int
    , maxLevel :: Int
    , minimumDamage :: Int
    , randomDamageMax :: Double
    , randomDamageMin :: Double
    , resetStagesOnSwitch :: Bool
    , sameAffinityBonus :: Double
    , simultaneousDefeatWins :: Bool
    , speedStat :: Text
    , stageLimit :: Int
    , stageMultiplierFormula :: Formula
    , statFormula :: Formula
    , statusCaptureBonuses :: Map Text Double
    , switchPriority :: Int
    , tieBreak :: Text
    , typeChart :: Map Text (Map Text Double)
    }
    deriving stock (Eq)
    deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec Rules)

instance HasCodec Rules where
    codec =
        Codec.bimapCodec validate id $
            Codec.object "Rules" $
                Rules
                    <$> Codec.optionalFieldWithOmittedDefault "accuracyFormula" (Formula "accuracy * accuracyStage / evasionStage") "Hit probability; the result is clamped to zero through one." Codec..= (.accuracyFormula)
                    <*> Codec.optionalFieldWithOmittedDefault "accuracyPerHit" False "Roll accuracy for each hit rather than once per move." Codec..= (.accuracyPerHit)
                    <*> Codec.optionalFieldWithOmittedDefault "accuracyStageFormula" (Formula "if(stage >= 0, (3 + stage) / 3, 3 / (3 - stage))") "Accuracy and evasion stage multiplier." Codec..= (.accuracyStageFormula)
                    <*> Codec.optionalFieldWithOmittedDefault "aiScoreFormula" (Formula "damage * accuracy + heal * if(hpRatio < 0.4, 2, 0.2) + status * 2") "Score used by the strongest opponent policy." Codec..= (.aiScoreFormula)
                    <*> Codec.optionalFieldWithOmittedDefault "captureFormula" (Formula "rate * itemBonus * (1 - hp / (2 * maxHP)) * statusBonus") "Capture probability; the result is clamped to zero through one." Codec..= (.captureFormula)
                    <*> Codec.optionalFieldWithOmittedDefault "captureGivesExperience" (False) "Whether a successful capture grants experience." Codec..= (.captureGivesExperience)
                    <*> Codec.optionalFieldWithOmittedDefault "criticalFormula" (Formula "criticalRate") "Critical probability." Codec..= (.criticalFormula)
                    <*> Codec.optionalFieldWithOmittedDefault "criticalIgnoresStages" (False) "Ignore attack and defense stages on critical hits." Codec..= (.criticalIgnoresStages)
                    <*> Codec.optionalFieldWithOmittedDefault "criticalMultiplierFormula" (Formula "2") "Critical damage multiplier." Codec..= (.criticalMultiplierFormula)
                    <*> Codec.optionalFieldWithOmittedDefault "criticalPerHit" True "Roll critical chance independently for each hit." Codec..= (.criticalPerHit)
                    <*> Codec.optionalFieldWithOmittedDefault "damageFormula" (Formula "max(1, floor(power * attack / defense / 5 * critical * affinity * sameAffinity * variance))") "Damage expression; type-chart immunity still prevents damage." Codec..= (.damageFormula)
                    <*> Codec.optionalFieldWithOmittedDefaultWith "defeatRecovery" NumberCodec.probabilityCodec (1) "Fraction of maximum health restored after defeat." Codec..= (.defeatRecovery)
                    <*> Codec.optionalFieldWithOmittedDefault "encountersAfterWarp" False "Allow encounter checks at warp destinations." Codec..= (.encountersAfterWarp)
                    <*> Codec.optionalFieldWithOmittedDefault "escapeFormula" (Formula "speed / max(1, enemySpeed) / 2 + attempts / 4") "Escape probability." Codec..= (.escapeFormula)
                    <*> Codec.optionalFieldWithOmittedDefault "escapePriority" (8) "Action priority for escape attempts." Codec..= (.escapePriority)
                    <*> Codec.optionalFieldWithOmittedDefault "experienceForFainted" False "Allow fainted recipients to gain experience." Codec..= (.experienceForFainted)
                    <*> Codec.optionalFieldWithOmittedDefault "experienceFormula" (Formula "level ^ 3") "Total experience threshold for a level." Codec..= (.experienceFormula)
                    <*> Codec.optionalFieldWithOmittedDefault "experienceRewardFormula" (Formula "floor(yield * level / max(1, participants))") "Experience per eligible party member." Codec..= (.experienceRewardFormula)
                    <*> Codec.optionalFieldWithOmittedDefault "experienceSharing" ("participants") "Experience recipients: participants or party." Codec..= (.experienceSharing)
                    <*> Codec.optionalFieldWithOmittedDefault "fallbackMove" (defaultMove) "Action used when no learned move has remaining uses." Codec..= (.fallbackMove)
                    <*> Codec.requiredField "healthFormula" "Maximum health on level changes and generated encounters." Codec..= (.healthFormula)
                    <*> Codec.optionalFieldWithOmittedDefault "itemPriority" (6) "Action priority for items." Codec..= (.itemPriority)
                    <*> Codec.optionalFieldWithOmittedDefault "learningPolicy" ("keep-newest") "Move overflow policy: keep-newest or keep-oldest." Codec..= (.learningPolicy)
                    <*> Codec.optionalFieldWithOmittedDefault "maxKnownMoves" (4) "Game-wide learned move limit." Codec..= (.maxKnownMoves)
                    <*> Codec.optionalFieldWithOmittedDefault "maxLevel" (100) "Game-wide level limit." Codec..= (.maxLevel)
                    <*> Codec.optionalFieldWithOmittedDefault "minimumDamage" (1) "Minimum damage of a nonimmune damaging hit." Codec..= (.minimumDamage)
                    <*> Codec.optionalFieldWithOmittedDefaultWith "randomDamageMax" NumberCodec.finiteDoubleCodec (1) "Upper damage variance multiplier." Codec..= (.randomDamageMax)
                    <*> Codec.optionalFieldWithOmittedDefaultWith "randomDamageMin" NumberCodec.finiteDoubleCodec (1) "Lower damage variance multiplier." Codec..= (.randomDamageMin)
                    <*> Codec.optionalFieldWithOmittedDefault "resetStagesOnSwitch" True "Clear temporary stages when switching." Codec..= (.resetStagesOnSwitch)
                    <*> Codec.optionalFieldWithOmittedDefaultWith "sameAffinityBonus" NumberCodec.finiteDoubleCodec (1.5) "Multiplier when a move matches the user's type." Codec..= (.sameAffinityBonus)
                    <*> Codec.optionalFieldWithOmittedDefault "simultaneousDefeatWins" False "Treat a simultaneous final faint as a victory." Codec..= (.simultaneousDefeatWins)
                    <*> Codec.optionalFieldWithOmittedDefault "speedStat" ("speed") "Stat used to order actions." Codec..= (.speedStat)
                    <*> Codec.optionalFieldWithOmittedDefault "stageLimit" (6) "Absolute limit on temporary stat stages." Codec..= (.stageLimit)
                    <*> Codec.optionalFieldWithOmittedDefault "stageMultiplierFormula" (Formula "if(stage >= 0, (2 + stage) / 2, 2 / (2 - stage))") "Temporary stat multiplier." Codec..= (.stageMultiplierFormula)
                    <*> Codec.requiredField "statFormula" "Combat stat calculated from its base value and level." Codec..= (.statFormula)
                    <*> Codec.optionalFieldWithOmittedDefaultWith "statusCaptureBonuses" (Codec.mapCodec NumberCodec.finiteDoubleCodec) (Map.empty) "Capture multipliers keyed by status." Codec..= (.statusCaptureBonuses)
                    <*> Codec.optionalFieldWithOmittedDefault "switchPriority" (7) "Action priority for a voluntary switch." Codec..= (.switchPriority)
                    <*> Codec.optionalFieldWithOmittedDefault "tieBreak" ("player") "Equal-speed order: player, opponent or random." Codec..= (.tieBreak)
                    <*> Codec.optionalFieldWithOmittedDefaultWith "typeChart" (Codec.mapCodec (Codec.mapCodec NumberCodec.finiteDoubleCodec)) (Map.empty) "Attack type to defending type multipliers." Codec..= (.typeChart)
      where
        validGrowth value = case traverse (\level -> Formula.evaluate (Map.singleton "level" (number level)) value.experienceFormula) [1 .. value.maxLevel] of
            Left _ -> False
            Right values -> all (>= 0) values && and (zipWith (<) (map integer values) (drop 1 (map integer values)))
        validate value
            | value.maxKnownMoves < 1 || value.maxLevel < 1 || value.maxLevel > 10000 = Left "Move and level limits must be positive; maxLevel must not exceed 10000."
            | value.fallbackMove.maxUses /= 0 = Left "The fallback move must have unlimited uses."
            | not (validGrowth value) = Left "Experience thresholds must be nonnegative, finite, strictly increasing and fit in an integer."
            | value.stageLimit < 0 || value.stageLimit > 100 || value.minimumDamage < 0 = Left "Stage limit must be between zero and 100 and minimum damage must be nonnegative."
            | value.randomDamageMin < 0 || value.randomDamageMax < value.randomDamageMin || value.sameAffinityBonus < 0 = Left "Damage multipliers must be nonnegative and ordered."
            | any (< 0) (concatMap Map.elems (Map.elems value.typeChart)) || any (< 0) (Map.elems value.statusCaptureBonuses) = Left "Affinity and capture multipliers must be nonnegative."
            | value.tieBreak `notElem` ["player", "opponent", "random"] || value.experienceSharing `notElem` ["participants", "party"] || value.learningPolicy `notElem` ["keep-newest", "keep-oldest"] = Left "Unknown battle policy."
            | either (const True) (any (`notElem` ["accuracy", "accuracyStage", "evasionStage", "level", "targetLevel"])) (Formula.variables value.accuracyFormula) = Left "Unknown variable in accuracyFormula."
            | either (const True) (any (`notElem` ["stage"])) (Formula.variables value.accuracyStageFormula) = Left "Unknown variable in accuracyStageFormula."
            | either (const True) (any (`notElem` ["damage", "accuracy", "heal", "hpRatio", "status", "priority", "targetHP"])) (Formula.variables value.aiScoreFormula) = Left "Unknown variable in aiScoreFormula."
            | either (const True) (any (`notElem` ["rate", "itemBonus", "hp", "maxHP", "statusBonus", "level", "targetLevel"])) (Formula.variables value.captureFormula) = Left "Unknown variable in captureFormula."
            | either (const True) (any (`notElem` ["criticalRate", "speed", "level", "targetLevel"])) (Formula.variables value.criticalFormula) = Left "Unknown variable in criticalFormula."
            | either (const True) (any (`notElem` ["level", "targetLevel"])) (Formula.variables value.criticalMultiplierFormula) = Left "Unknown variable in criticalMultiplierFormula."
            | either (const True) (any (`notElem` ["power", "attack", "defense", "baseAttack", "baseDefense", "critical", "affinity", "sameAffinity", "variance", "level", "targetLevel"])) (Formula.variables value.damageFormula) = Left "Unknown variable in damageFormula."
            | either (const True) (any (`notElem` ["speed", "enemySpeed", "attempts", "level", "enemyLevel"])) (Formula.variables value.escapeFormula) = Left "Unknown variable in escapeFormula."
            | either (const True) (any (`notElem` ["level"])) (Formula.variables value.experienceFormula) = Left "Unknown variable in experienceFormula."
            | either (const True) (any (`notElem` ["yield", "level", "participants"])) (Formula.variables value.experienceRewardFormula) = Left "Unknown variable in experienceRewardFormula."
            | either (const True) (any (`notElem` ["base", "level"])) (Formula.variables value.healthFormula) = Left "Unknown variable in healthFormula."
            | either (const True) (any (`notElem` ["stage"])) (Formula.variables value.stageMultiplierFormula) = Left "Unknown variable in stageMultiplierFormula."
            | either (const True) (any (`notElem` ["base", "level"])) (Formula.variables value.statFormula) = Left "Unknown variable in statFormula."
            | otherwise = Right value
