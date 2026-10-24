module Critters.State.Battle (Battle (..), defaultBattle) where

import Autodocodec qualified as Codec
import Autodocodec (Autodocodec (..), HasCodec)
import Critters.State.BattleDefinition (BattleDefinition, defaultDefinition)
import Critters.State.BattleOutcome (BattleOutcome)
import Critters.State.BattlePhase (BattlePhase (ActionMenu))
import Critters.State.CombatConfig (CombatConfig, defaultCombat)
import Critters.State.CombatConfig qualified as Config
import Critters.State.Critter (Critter)
import Critters.State.Rules qualified as Rules
import Data.Aeson qualified as Aeson
import Data.List (nub)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)

data Battle = Battle
  { battleActiveOpponent :: Int,
    battleActivePlayer :: Int,
    battleCapturedOpponents :: [Int],
    battleConfig :: CombatConfig,
    battleDefinition :: BattleDefinition,
    battleEnemyStages :: Map Text Int,
    battleEscapeAttempts :: Int,
    battleInitialSeed :: Maybe Int,
    battleMessages :: [Text],
    battleOpponentParty :: [Critter],
    battleOutcome :: Maybe BattleOutcome,
    battleParticipants :: [Int],
    battlePhase :: BattlePhase,
    battlePlayerStages :: Map Text Int,
    battleRewardedOpponents :: [Int],
    battleSelected :: Int,
    battleSelectedItem :: Maybe Text,
    battleTurn :: Int
  }
  deriving stock (Eq)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec Battle)

defaultBattle :: Battle
defaultBattle = Battle
  { battleActiveOpponent = 0,
    battleActivePlayer = 0,
    battleCapturedOpponents = [],
    battleConfig = defaultCombat,
    battleDefinition = defaultDefinition,
    battleEnemyStages = Map.empty,
    battleEscapeAttempts = 0,
    battleInitialSeed = Nothing,
    battleMessages = [],
    battleOpponentParty = [],
    battleOutcome = Nothing,
    battleParticipants = [],
    battlePhase = ActionMenu,
    battlePlayerStages = Map.empty,
    battleRewardedOpponents = [],
    battleSelected = 0,
    battleSelectedItem = Nothing,
    battleTurn = 0
  }

instance HasCodec Battle where
  codec = Codec.bimapCodec validate id $ Codec.object "Battle" $ Battle
    <$> Codec.optionalFieldWithOmittedDefault "activeOpponent" (0) "Active opposing party index." Codec..= battleActiveOpponent
    <*> Codec.optionalFieldWithOmittedDefault "activePlayer" (0) "Active player party index." Codec..= battleActivePlayer
    <*> Codec.optionalFieldWithOmittedDefault "capturedOpponents" [] "Opponents captured during this battle." Codec..= battleCapturedOpponents
    <*> Codec.optionalFieldWithOmittedDefault "config" (defaultCombat) "Configuration snapshot selected at entry." Codec..= battleConfig
    <*> Codec.optionalFieldWithOmittedDefault "definition" (defaultDefinition) "Battle definition snapshot." Codec..= battleDefinition
    <*> Codec.optionalFieldWithOmittedDefault "enemyStages" (Map.empty) "Temporary opposing stat stages." Codec..= battleEnemyStages
    <*> Codec.optionalFieldWithOmittedDefault "escapeAttempts" (0) "Failed escape attempts." Codec..= battleEscapeAttempts
    <*> Codec.optionalFieldOrNull "initialSeed" "Random state at entry; absent in older histories." Codec..= battleInitialSeed
    <*> Codec.optionalFieldWithOmittedDefault "messages" ([]) "Messages from the most recent action." Codec..= battleMessages
    <*> Codec.optionalFieldWithOmittedDefault "opponentParty" ([]) "Current opposing party." Codec..= battleOpponentParty
    <*> Codec.optionalFieldOrNull "outcome" "Resolved outcome awaiting acknowledgement." Codec..= battleOutcome
    <*> Codec.optionalFieldWithOmittedDefault "participants" ([]) "Player party indices that have participated." Codec..= battleParticipants
    <*> Codec.optionalFieldWithOmittedDefault "phase" (ActionMenu) "Current battle menu." Codec..= battlePhase
    <*> Codec.optionalFieldWithOmittedDefault "playerStages" (Map.empty) "Temporary player stat stages." Codec..= battlePlayerStages
    <*> Codec.optionalFieldWithOmittedDefault "rewardedOpponents" [] "Opponent indices whose experience has been resolved." Codec..= battleRewardedOpponents
    <*> Codec.optionalFieldWithOmittedDefault "selected" (0) "Selected menu entry." Codec..= battleSelected
    <*> Codec.optionalFieldOrNull "selectedItem" "Item awaiting a party target." Codec..= battleSelectedItem
    <*> Codec.optionalFieldWithOmittedDefault "turn" (0) "Completed turns." Codec..= battleTurn
    where
      validate value
        | battleActiveOpponent value < 0 || battleActiveOpponent value >= length (battleOpponentParty value) = Left "Active opponent index is outside its party."
        | battleActivePlayer value < 0 || battleEscapeAttempts value < 0 || battleSelected value < 0 || battleTurn value < 0 || any (< 0) (battleParticipants value) = Left "Battle indices and counts must not be negative."
        | maybe False (\seed -> seed < 0 || seed > 2147483647) (battleInitialSeed value) = Left "Invalid battle entry seed."
        | length (nub (battleParticipants value)) /= length (battleParticipants value) = Left "Battle participants must be unique."
        | any (\index -> index < 0 || index >= length (battleOpponentParty value)) (battleCapturedOpponents value <> battleRewardedOpponents value) = Left "Recorded opponent indices are outside the opposing party."
        | any (\stage -> stage < negate (Rules.rulesStageLimit (Config.configRules (battleConfig value))) || stage > Rules.rulesStageLimit (Config.configRules (battleConfig value))) (Map.elems (battlePlayerStages value) <> Map.elems (battleEnemyStages value)) = Left "Battle stages exceed the configured limit."
        | otherwise = Right value
