module Critters.Battle.Menu (entries, Selection (..)) where

import Critters.Battle.Action (Action (..))
import Critters.Battle.Common qualified as Common
import Critters.State.Battle qualified as Battle
import Critters.State.BattleDefinition qualified as Definition
import Critters.State.BattlePhase (BattlePhase (..))
import Critters.State.CombatConfig qualified as Config
import Critters.State.Critter qualified as Critter
import Critters.State.GameState (GameState (..))
import Critters.State.Item qualified as Item
import Critters.State.Move qualified as Move
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Witch (from)

entries :: GameState -> [(Text, Selection)]
entries state = case Common.contextFor state of
  Nothing -> []
  Just context -> case Battle.battlePhase (Common.contextBattle context) of
    ActionMenu -> [("Moves", GoTo MoveMenu), ("Party", GoTo SwitchMenu), ("Items", GoTo ItemMenu)] <> [("Escape", Perform EscapeBattle) | Definition.definitionAllowEscape (Battle.battleDefinition (Common.contextBattle context))]
    Finished -> [("Continue", Finish)]
    ItemMenu -> mapMaybe (itemEntry context) (Map.toAscList (inventory state))
    ItemTargetMenu -> partyEntries context True
    MoveMenu -> [(Move.moveName move <> resource move identifier context, Perform (UseMove identifier)) | (identifier, move) <- Common.availableMoves Common.Player context]
    ReplacementMenu -> partyEntries context False
    SwitchMenu -> partyEntries context False
  where
    itemEntry context (identifier, count)
      | count <= 0 = Nothing
      | otherwise = do
          definition <- Map.lookup identifier (Config.configItems (Battle.battleConfig (Common.contextBattle context)))
          if Item.itemCaptureBonus definition > 0 && not (Definition.definitionAllowCapture (Battle.battleDefinition (Common.contextBattle context))) then Nothing else pure ((if Item.itemName definition == "" then identifier else Item.itemName definition) <> " x" <> from (show count), if Item.itemCaptureBonus definition > 0 then Perform (UseItem identifier (-1)) else PickItem identifier)
    partyEntries context targeting =
      [ (Common.displayName context critter <> "  HP " <> from (show (Critter.hitPoints critter)) <> "/" <> from (show (Critter.maxHitPoints critter)), Perform (if targeting then UseItem (maybe "" id (Battle.battleSelectedItem (Common.contextBattle context))) index else SwitchCritter index))
      | (index, critter) <- zip [0 ..] (Common.allCritters Common.Player context),
        targeting || Battle.battlePhase (Common.contextBattle context) == SwitchMenu || (index /= Common.activeIndex Common.Player context && Common.usable critter)
      ]
    resource move identifier context
      | Move.moveMaxUses move == 0 = ""
      | otherwise = "  " <> from (show (maybe 0 (\critter -> Common.moveRemaining critter identifier move) (Common.active Common.Player context))) <> "/" <> from (show (Move.moveMaxUses move))

data Selection = Finish | GoTo BattlePhase | Perform Action | PickItem Text
