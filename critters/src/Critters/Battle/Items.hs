module Critters.Battle.Items (escape, useItem, validateItem) where

import Control.Monad.Trans.State.Strict (State)
import Control.Monad.Trans.State.Strict qualified as State
import Critters.Battle.Common (Context (..), Side (..))
import Critters.Battle.Common qualified as Common
import Critters.Number (integer, number)
import Critters.State.Affliction qualified as Affliction
import Critters.State.Battle (Battle (..))
import Critters.State.BattleDefinition qualified as Definition
import Critters.State.BattleOutcome (BattleOutcome (..))
import Critters.State.BattleProfile qualified as Profile
import Critters.State.CombatConfig qualified as Config
import Critters.State.Critter (Critter)
import Critters.State.Critter qualified as Critter
import Critters.State.Item (Item)
import Critters.State.Item qualified as Item
import Critters.State.Move qualified as Move
import Critters.State.Party (Party (..))
import Critters.State.Rules qualified as Rules
import Critters.State.Species qualified as Species
import Data.Map.Strict qualified as Map
import Data.Text (Text)

escape :: State Context ()
escape = do
  context <- State.get
  let rules = Common.rulesFor context
      probability = Common.evaluate 0 (Rules.rulesEscapeFormula rules)
        [ ("attempts", number (battleEscapeAttempts (contextBattle context))),
          ("enemyLevel", maybe 1 (number . Critter.level) (Common.active Enemy context)),
          ("enemySpeed", Common.stat context Enemy (Rules.rulesSpeedStat rules)),
          ("level", maybe 1 (number . Critter.level) (Common.active Player context)),
          ("speed", Common.stat context Player (Rules.rulesSpeedStat rules))
        ]
  escaped <- Common.chance probability
  if escaped then Common.endBattle Escaped
  else do
    State.modify $ \current -> current{contextBattle = (contextBattle current){battleEscapeAttempts = battleEscapeAttempts (contextBattle current) + 1}}
    Common.emit "The escape attempt failed."

restore :: Context -> Item -> Critter -> Critter
restore context item critter = critter
  { Critter.battleProfile = profile{Profile.profileRemainingUses = resources, Profile.profileStatus = if Item.itemCureStatus item then Nothing else Profile.profileStatus profile},
    Critter.hitPoints = health
  }
  where
    health
      | Critter.hitPoints critter == 0 = if Item.itemReviveFraction item > 0 then max 1 (integer (number (Critter.maxHitPoints critter) * Item.itemReviveFraction item)) else 0
      | otherwise = min (Critter.maxHitPoints critter) (integer (number (Critter.hitPoints critter) + number (Item.itemHealing item) + number (Critter.maxHitPoints critter) * Item.itemHealthFraction item))
    profile = Critter.battleProfile critter
    resources = foldl' replenish (Profile.profileRemainingUses profile) (Common.knownMoves (contextBattle context) critter)
    replenish remaining identifier = case Map.lookup identifier (Config.configMoves (battleConfig (contextBattle context))) of
      Just move | Move.moveMaxUses move > 0 && Item.itemRestoreUses item > 0 ->
        let old = Common.moveRemaining critter identifier move
            next = min (Move.moveMaxUses move) (integer (number old + number (Item.itemRestoreUses item)))
         in if next == old then remaining else Map.insert identifier next remaining
      _ -> remaining

useItem :: Text -> Int -> State Context ()
useItem identifier target = do
  context <- State.get
  case validateItem context identifier target of
    Left message -> Common.emit message
    Right item -> do
      State.modify $ \current -> current{contextInventory = Map.adjust (subtract 1) identifier (contextInventory current)}
      if Item.itemCaptureBonus item > 0 then capture item
      else case drop target (members (contextParty context)) of
        critter : _ -> do
          State.modify (Common.setCritter Player target (restore context item critter))
          Common.emit ("Used " <> (if Item.itemName item == "" then identifier else Item.itemName item) <> " on " <> Critter.species critter <> ".")
        [] -> pure ()
  where
    capture item = do
      context <- State.get
      case Common.active Enemy context of
        Nothing -> pure ()
        Just critter -> do
          let config = battleConfig (contextBattle context)
              rate = maybe 0 Species.speciesCaptureRate (Map.lookup (Critter.species critter) (Config.configSpecies config))
              bonus = maybe 1 (\affliction -> Map.findWithDefault 1 (Affliction.afflictionStatus affliction) (Rules.rulesStatusCaptureBonuses (Common.rulesFor context))) (Profile.profileStatus (Critter.battleProfile critter))
              probability = Common.evaluate 0 (Rules.rulesCaptureFormula (Common.rulesFor context))
                [ ("hp", number (Critter.hitPoints critter)), ("itemBonus", Item.itemCaptureBonus item),
                  ("level", maybe 1 (number . Critter.level) (Common.active Player context)),
                  ("maxHP", number (Critter.maxHitPoints critter)), ("rate", rate), ("statusBonus", bonus), ("targetLevel", number (Critter.level critter))
                ]
          captured <- Common.chance probability
          if not captured then Common.emit "The capture attempt failed."
          else do
            State.modify $ \current ->
              let updated = Common.setCritter Enemy (Common.activeIndex Enemy current) critter{Critter.hitPoints = 0} current
               in updated{contextParty = Party (members (contextParty current) <> [critter]), contextBattle = (contextBattle updated){battleCapturedOpponents = battleCapturedOpponents (contextBattle current) <> [Common.activeIndex Enemy current]}}
            Common.emit ("Captured " <> Critter.species critter <> ".")
            remaining <- State.gets (any Common.usable . Common.allCritters Enemy)
            if remaining then pure () else Common.endBattle Captured

validateItem :: Context -> Text -> Int -> Either Text Item
validateItem context identifier target = do
  item <- maybe (Left "That item is not defined.") Right (Map.lookup identifier (Config.configItems (battleConfig (contextBattle context))))
  if Map.findWithDefault 0 identifier (contextInventory context) <= 0 then Left "You have none of that item." else pure ()
  if Item.itemCaptureBonus item > 0 then do
    if not (Definition.definitionAllowCapture (battleDefinition (contextBattle context))) then Left "Capture is not permitted in this battle." else pure ()
    if length (members (contextParty context)) >= contextLimit context then Left "Your party is full." else pure ()
    if maybe False Common.usable (Common.active Enemy context) then Right item else Left "There is no opponent to capture."
  else if target < 0 then Left "Choose a party member."
  else case drop target (members (contextParty context)) of
    critter : _ | restore context item critter /= critter -> Right item
    _ -> Left "That item would have no effect."
