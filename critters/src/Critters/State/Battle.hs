module Critters.State.Battle (Battle (..), defaultBattle) where

import Autodocodec (Autodocodec (..), HasCodec)
import Autodocodec qualified as Codec
import Critters.State.BattleDefinition (BattleDefinition, defaultDefinition)
import Critters.State.BattleOutcome (BattleOutcome)
import Critters.State.BattlePhase (BattlePhase (ActionMenu))
import Critters.State.CombatConfig (CombatConfig, defaultCombat)
import Critters.State.CombatConfig qualified as Config
import Critters.State.Critter (Critter)
import Critters.State.Message (BattleMessage, Messages (..))
import Critters.State.Stage (StageDefinition (..), StageValues (..), stageDefinition)
import Data.Aeson qualified as Aeson
import Data.List (nub)
import Data.Map.Strict qualified as Map
import Data.Text (Text)

data Battle = Battle
    { activeOpponent :: Int
    , activePlayer :: Int
    , capturedOpponents :: [Int]
    , config :: CombatConfig
    , definition :: BattleDefinition
    , enemyStages :: StageValues
    , escapeAttempts :: Int
    , initialSeed :: Maybe Int
    , introductionIndex :: Int
    , messages :: [BattleMessage]
    , messageTemplates :: Messages
    , opponentParty :: [Critter]
    , outcome :: Maybe BattleOutcome
    , participants :: [Int]
    , phase :: BattlePhase
    , playerStages :: StageValues
    , rewardedOpponents :: [Int]
    , selected :: Int
    , selectedItem :: Maybe Text
    , turn :: Int
    }
    deriving stock (Eq)
    deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec Battle)

defaultBattle :: Battle
defaultBattle =
    Battle
        { activeOpponent = 0
        , activePlayer = 0
        , capturedOpponents = []
        , config = defaultCombat
        , definition = defaultDefinition
        , enemyStages = StageValues Map.empty
        , escapeAttempts = 0
        , initialSeed = Nothing
        , introductionIndex = 0
        , messages = []
        , messageTemplates = Messages Map.empty
        , opponentParty = []
        , outcome = Nothing
        , participants = []
        , phase = ActionMenu
        , playerStages = StageValues Map.empty
        , rewardedOpponents = []
        , selected = 0
        , selectedItem = Nothing
        , turn = 0
        }

instance HasCodec Battle where
    codec =
        Codec.bimapCodec validate id $
            Codec.object "Battle" $
                Battle
                    <$> Codec.optionalFieldWithOmittedDefault "activeOpponent" (0) "Active opposing party index." Codec..= (.activeOpponent)
                    <*> Codec.optionalFieldWithOmittedDefault "activePlayer" (0) "Active player party index." Codec..= (.activePlayer)
                    <*> Codec.optionalFieldWithOmittedDefault "capturedOpponents" [] "Opponents captured during this battle." Codec..= (.capturedOpponents)
                    <*> Codec.optionalFieldWithOmittedDefault "config" (defaultCombat) "Configuration snapshot selected at entry." Codec..= (.config)
                    <*> Codec.optionalFieldWithOmittedDefault "definition" (defaultDefinition) "Battle definition snapshot." Codec..= (.definition)
                    <*> Codec.optionalFieldWithOmittedDefault "enemyStages" (StageValues Map.empty) "Temporary opposing stat stages." Codec..= (.enemyStages)
                    <*> Codec.optionalFieldWithOmittedDefault "escapeAttempts" (0) "Failed escape attempts." Codec..= (.escapeAttempts)
                    <*> Codec.optionalFieldOrNull "initialSeed" "Random state at entry; absent in older histories." Codec..= (.initialSeed)
                    <*> Codec.optionalFieldWithOmittedDefault "introductionIndex" 0 "Current pre-battle dialogue line." Codec..= (.introductionIndex)
                    <*> Codec.optionalFieldWithOmittedDefault "messages" ([]) "Messages from the most recent action." Codec..= (.messages)
                    <*> Codec.requiredField "messageTemplates" "Root message catalogue captured at battle entry." Codec..= (.messageTemplates)
                    <*> Codec.optionalFieldWithOmittedDefault "opponentParty" ([]) "Current opposing party." Codec..= (.opponentParty)
                    <*> Codec.optionalFieldOrNull "outcome" "Resolved outcome awaiting acknowledgement." Codec..= (.outcome)
                    <*> Codec.optionalFieldWithOmittedDefault "participants" ([]) "Player party indices that have participated." Codec..= (.participants)
                    <*> Codec.optionalFieldWithOmittedDefault "phase" (ActionMenu) "Current battle menu." Codec..= (.phase)
                    <*> Codec.optionalFieldWithOmittedDefault "playerStages" (StageValues Map.empty) "Temporary player stat stages." Codec..= (.playerStages)
                    <*> Codec.optionalFieldWithOmittedDefault "rewardedOpponents" [] "Opponent indices whose experience has been resolved." Codec..= (.rewardedOpponents)
                    <*> Codec.optionalFieldWithOmittedDefault "selected" (0) "Selected menu entry." Codec..= (.selected)
                    <*> Codec.optionalFieldOrNull "selectedItem" "Item awaiting a party target." Codec..= (.selectedItem)
                    <*> Codec.optionalFieldWithOmittedDefault "turn" (0) "Completed turns." Codec..= (.turn)
      where
        outsideLimit definitions (name, stage) =
            let limit = (stageDefinition definitions name).limit
             in stage < negate limit || stage > limit
        validate value
            | value.activeOpponent < 0 || value.activeOpponent >= length value.opponentParty = Left "Active opponent index is outside its party."
            | value.introductionIndex < 0 || value.activePlayer < 0 || value.escapeAttempts < 0 || value.selected < 0 || value.turn < 0 || any (< 0) value.participants = Left "Battle indices and counts must not be negative."
            | maybe False (\seed -> seed < 0 || seed > 2147483647) value.initialSeed = Left "Invalid battle entry seed."
            | length (nub value.participants) /= length value.participants = Left "Battle participants must be unique."
            | any (\index -> index < 0 || index >= length value.opponentParty) (value.capturedOpponents <> value.rewardedOpponents) = Left "Recorded opponent indices are outside the opposing party."
            | any (outsideLimit value.config.stages) (Map.toList value.playerStages.values <> Map.toList value.enemyStages.values) = Left "Battle stages exceed the configured limit."
            | otherwise = Right value
