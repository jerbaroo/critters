module Critters.Battle.Menu (entries, Selection (..)) where

import Critters.Battle.Action (Action (..))
import Critters.Battle.Common qualified as Common
import Critters.Message qualified as Message
import Critters.State.Battle qualified as Battle
import Critters.State.BattleDefinition qualified as Definition
import Critters.State.BattlePhase (BattlePhase (..))
import Critters.State.CombatConfig qualified as Config
import Critters.State.Critter qualified as Critter
import Critters.State.GameState (GameState (..))
import Critters.State.GameState qualified as GameState
import Critters.State.Item qualified as Item
import Critters.State.Move qualified as Move
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)

entries :: GameState -> [(Text, Selection)]
entries state = case Common.contextFor state of
    Nothing -> []
    Just context -> case context.battle.phase of
        ActionMenu -> [(label "menuMoves" [], GoTo MoveMenu), (label "menuParty" [], GoTo SwitchMenu), (label "menuItems" [], GoTo ItemMenu)] <> [(label "menuEscape" [], Perform EscapeBattle) | context.battle.definition.allowEscape]
        Finished -> []
        Introduction -> []
        ItemMenu -> mapMaybe (itemEntry context) (Map.toAscList state.inventory)
        ItemTargetMenu -> partyEntries context True
        MoveMenu -> [(label "menuMove" [("move", Message.reference move.name), ("resource", Message.text (resource move identifier context))], Perform (UseMove identifier)) | (identifier, move) <- Common.availableMoves Common.Player context]
        ReplacementMenu -> partyEntries context False
        SwitchMenu -> partyEntries context False
  where
    label = GameState.messageText state
    itemEntry context (identifier, count)
        | count <= 0 = Nothing
        | otherwise = do
            definition <- Map.lookup identifier context.battle.config.items
            if definition.captureBonus > 0 && not context.battle.definition.allowCapture then Nothing else pure (label "menuItem" [("item", Message.reference ("{combat.items." <> identifier <> ".name}")), ("count", Message.numeric count)], if definition.captureBonus > 0 then Perform (UseItem identifier (-1)) else PickItem identifier)
    partyEntries context targeting =
        [ (label "menuPartyMember" [("species", Common.critterName critter), ("hp", Message.numeric critter.hitPoints), ("maxHP", Message.numeric critter.maxHitPoints)], Perform (if targeting then UseItem (maybe "" id context.battle.selectedItem) index else SwitchCritter index))
        | (index, critter) <- zip [0 ..] (Common.allCritters Common.Player context)
        , targeting || context.battle.phase == SwitchMenu || (index /= Common.activeIndex Common.Player context && Common.usable critter)
        ]
    resource move identifier context
        | move.maxUses == 0 = ""
        | otherwise = label "menuMoveResource" [("remaining", Message.numeric (maybe 0 (\critter -> Common.moveRemaining critter identifier move) (Common.active Common.Player context))), ("maximum", Message.numeric move.maxUses)]

data Selection = GoTo BattlePhase | Perform Action | PickItem Text
