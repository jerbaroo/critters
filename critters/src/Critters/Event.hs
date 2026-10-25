module Critters.Event (Event (..)) where

import Autodocodec (Autodocodec (..), HasCodec, JSONObjectCodec)
import Autodocodec qualified as Codec
import Critters.Input.Direction (Direction)
import Critters.Pretty (Pretty (..))
import Critters.State.ActiveInteraction (ActiveInteraction)
import Critters.State.Battle (Battle)
import Critters.State.BattleSide (BattleSide)
import Critters.State.BattleUpdate (BattleUpdate)
import Critters.State.Critter (Critter)
import Critters.State.Critter qualified as Critter
import Critters.State.FlagId (FlagId)
import Critters.State.Location (Location)
import Critters.State.MoveId (MoveId)
import Critters.State.NpcId (NpcId)
import Critters.State.ObjectId (ObjectId)
import Critters.State.Party (Party)
import Critters.State.Position (Position)
import Data.Aeson qualified as Aeson
import Data.HashMap.Strict qualified as HashMap
import Data.Text (Text)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy.Builder.Int (decimal)
import Witch (from)

-- | An established fact. Validation happens before an event is produced.
data Event
  = BattleChanged BattleUpdate
  | BattleCombatantsSentOut
  | BattleEnded
  | BattleIntroductionAdvanced Int
  | BattleStarted Battle
  | ConversationAdvanced Int
  | ConversationEnded
  | ConversationStarted NpcId
  | CritterReceived Critter
  | EncounterAdvanced Int
  | FlagChanged FlagId Bool
  | InteractionAdvanced Int
  | InteractionChoiceOpened
  | InteractionChoiceSelected Int
  | InteractionEnded
  | InteractionStarted ActiveInteraction
  | InventoryChanged Text Int
  | MoveUsed BattleSide MoveId BattleUpdate
  | NpcMoved NpcId Location
  | ObjectRemoved ObjectId
  | PartyChanged Party
  | PartyClosed
  | PartyOpened
  | PlayerMoved Position
  | PlayerTurned Direction
  | PlayerWarped Location
  | RandomAdvanced Int
  | TurnCompleted BattleUpdate
  deriving stock (Eq)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec Event)

instance HasCodec Event where
  codec =
    Codec.object "Event"
      $ Codec.discriminatedUnionCodec "type" encode
      $ HashMap.fromList
        [ ("battle-changed", ("BattleChanged", Codec.mapToDecoder BattleChanged battleChanged)),
          ("battle-combatants-sent-out", ("BattleCombatantsSentOut", pure BattleCombatantsSentOut)),
          ("battle-ended", ("BattleEnded", pure BattleEnded)),
          ("battle-introduction-advanced", ("BattleIntroductionAdvanced", Codec.mapToDecoder BattleIntroductionAdvanced advanced)),
          ("battle-started", ("BattleStarted", Codec.mapToDecoder BattleStarted battleStarted)),
          ("conversation-advanced", ("ConversationAdvanced", Codec.mapToDecoder ConversationAdvanced advanced)),
          ("conversation-ended", ("ConversationEnded", pure ConversationEnded)),
          ("conversation-started", ("ConversationStarted", Codec.mapToDecoder ConversationStarted started)),
          ("critter-received", ("CritterReceived", Codec.mapToDecoder CritterReceived given)),
          ("encounter-advanced", ("EncounterAdvanced", Codec.mapToDecoder EncounterAdvanced encountered)),
          ("flag-changed", ("FlagChanged", Codec.mapToDecoder (uncurry FlagChanged) changed)),
          ("interaction-advanced", ("InteractionAdvanced", Codec.mapToDecoder InteractionAdvanced progressed)),
          ("interaction-choice-opened", ("InteractionChoiceOpened", pure InteractionChoiceOpened)),
          ("interaction-choice-selected", ("InteractionChoiceSelected", Codec.mapToDecoder InteractionChoiceSelected chosen)),
          ("interaction-ended", ("InteractionEnded", pure InteractionEnded)),
          ("interaction-started", ("InteractionStarted", Codec.mapToDecoder InteractionStarted interaction)),
          ("inventory-changed", ("InventoryChanged", Codec.mapToDecoder (uncurry InventoryChanged) inventoryChanged)),
          ("move-used", ("MoveUsed", Codec.mapToDecoder (\(side, identifier, update) -> MoveUsed side identifier update) moveUsed)),
          ("npc-moved", ("NpcMoved", Codec.mapToDecoder (uncurry NpcMoved) npcMoved)),
          ("object-removed", ("ObjectRemoved", Codec.mapToDecoder ObjectRemoved removed)),
          ("party-changed", ("PartyChanged", Codec.mapToDecoder PartyChanged partyChanged)),
          ("party-closed", ("PartyClosed", pure PartyClosed)),
          ("party-opened", ("PartyOpened", pure PartyOpened)),
          ("player-moved", ("PlayerMoved", Codec.mapToDecoder PlayerMoved moved)),
          ("player-turned", ("PlayerTurned", Codec.mapToDecoder PlayerTurned turned)),
          ("player-warped", ("PlayerWarped", Codec.mapToDecoder PlayerWarped warped)),
          ("random-advanced", ("RandomAdvanced", Codec.mapToDecoder RandomAdvanced randomized)),
          ("turn-completed", ("TurnCompleted", Codec.mapToDecoder TurnCompleted battleChanged))
        ]
    where
      advanced :: JSONObjectCodec Int
      advanced = Codec.requiredField "lineIndex" "The new dialogue line index."
      battleChanged :: JSONObjectCodec BattleUpdate
      battleChanged = Codec.requiredField "update" "Resolved battle facts."
      battleStarted :: JSONObjectCodec Battle
      battleStarted = Codec.requiredField "battle" "The battle entry snapshot."
      changed :: JSONObjectCodec (FlagId, Bool)
      changed =
        (,)
          <$> Codec.requiredField "flag" "The game-defined flag identifier." Codec..= fst
          <*> Codec.requiredField "value" "The new flag value." Codec..= snd
      chosen :: JSONObjectCodec Int
      chosen = Codec.requiredField "choiceIndex" "The selected interaction choice index."
      encode = \case
        BattleChanged update -> ("battle-changed", Codec.mapToEncoder update battleChanged)
        BattleCombatantsSentOut -> ("battle-combatants-sent-out", pure ())
        BattleEnded -> ("battle-ended", pure ())
        BattleIntroductionAdvanced index -> ("battle-introduction-advanced", Codec.mapToEncoder index advanced)
        BattleStarted active -> ("battle-started", Codec.mapToEncoder active battleStarted)
        ConversationAdvanced index -> ("conversation-advanced", Codec.mapToEncoder index advanced)
        ConversationEnded -> ("conversation-ended", pure ())
        ConversationStarted identifier -> ("conversation-started", Codec.mapToEncoder identifier started)
        CritterReceived critter -> ("critter-received", Codec.mapToEncoder critter given)
        EncounterAdvanced count -> ("encounter-advanced", Codec.mapToEncoder count encountered)
        FlagChanged identifier value -> ("flag-changed", Codec.mapToEncoder (identifier, value) changed)
        InteractionAdvanced index -> ("interaction-advanced", Codec.mapToEncoder index progressed)
        InteractionChoiceOpened -> ("interaction-choice-opened", pure ())
        InteractionChoiceSelected index -> ("interaction-choice-selected", Codec.mapToEncoder index chosen)
        InteractionEnded -> ("interaction-ended", pure ())
        InteractionStarted active -> ("interaction-started", Codec.mapToEncoder active interaction)
        InventoryChanged identifier count -> ("inventory-changed", Codec.mapToEncoder (identifier, count) inventoryChanged)
        MoveUsed side identifier update -> ("move-used", Codec.mapToEncoder (side, identifier, update) moveUsed)
        NpcMoved identifier destination -> ("npc-moved", Codec.mapToEncoder (identifier, destination) npcMoved)
        ObjectRemoved identifier -> ("object-removed", Codec.mapToEncoder identifier removed)
        PartyChanged value -> ("party-changed", Codec.mapToEncoder value partyChanged)
        PartyClosed -> ("party-closed", pure ())
        PartyOpened -> ("party-opened", pure ())
        PlayerMoved destination -> ("player-moved", Codec.mapToEncoder destination moved)
        PlayerTurned direction -> ("player-turned", Codec.mapToEncoder direction turned)
        PlayerWarped destination -> ("player-warped", Codec.mapToEncoder destination warped)
        RandomAdvanced seed -> ("random-advanced", Codec.mapToEncoder seed randomized)
        TurnCompleted update -> ("turn-completed", Codec.mapToEncoder update battleChanged)
      encountered :: JSONObjectCodec Int
      encountered = Codec.requiredField "steps" "Eligible encounter steps."
      given :: JSONObjectCodec Critter
      given = Codec.requiredField "critter" "The critter received by the player."
      interaction :: JSONObjectCodec ActiveInteraction
      interaction = Codec.requiredField "interaction" "The selected interaction snapshot and its initial presentation."
      inventoryChanged :: JSONObjectCodec (Text, Int)
      inventoryChanged = (,) <$> Codec.requiredField "item" "Item identifier." Codec..= fst <*> Codec.requiredField "count" "Resolved remaining count." Codec..= snd
      moved :: JSONObjectCodec Position
      moved = Codec.requiredField "position" "The player's position after moving."
      moveUsed :: JSONObjectCodec (BattleSide, MoveId, BattleUpdate)
      moveUsed =
        (,,)
          <$> Codec.requiredField "side" "The side that used the move." Codec..= (\(side, _, _) -> side)
          <*> Codec.requiredField "move" "The move that was used." Codec..= (\(_, identifier, _) -> identifier)
          <*> battleChanged Codec..= (\(_, _, update) -> update)
      npcMoved :: JSONObjectCodec (NpcId, Location)
      npcMoved = (,) <$> Codec.requiredField "npc" "The NPC that moved." Codec..= fst <*> Codec.requiredField "location" "Resolved NPC location." Codec..= snd
      partyChanged :: JSONObjectCodec Party
      partyChanged = Codec.requiredField "party" "Resolved party."
      progressed :: JSONObjectCodec Int
      progressed = Codec.requiredField "dialogueIndex" "The new scripted dialogue line index."
      randomized :: JSONObjectCodec Int
      randomized = Codec.requiredField "seed" "Advanced random state."
      removed :: JSONObjectCodec ObjectId
      removed = Codec.requiredField "objectId" "The world object that was removed."
      started :: JSONObjectCodec NpcId
      started = Codec.requiredField "npcId" "The NPC whose conversation started."
      turned :: JSONObjectCodec Direction
      turned = Codec.requiredField "direction" "The direction the player now faces."
      warped :: JSONObjectCodec Location
      warped = Codec.requiredField "location" "The map and coordinates the player arrived at."

instance Pretty Event where
  pretty = \case
    BattleChanged _ -> "BattleChanged"
    BattleCombatantsSentOut -> "BattleCombatantsSentOut"
    BattleEnded -> "BattleEnded"
    BattleIntroductionAdvanced index -> "BattleIntroductionAdvanced " <> from (toLazyText (decimal index))
    BattleStarted _ -> "BattleStarted"
    ConversationAdvanced index -> from $ toLazyText $ "ConversationAdvanced " <> decimal index
    ConversationEnded -> "ConversationEnded"
    ConversationStarted identifier -> "ConversationStarted " <> pretty identifier
    CritterReceived critter -> "CritterReceived " <> Critter.species critter
    EncounterAdvanced index -> from $ toLazyText $ "EncounterAdvanced " <> decimal index
    FlagChanged identifier value -> "FlagChanged " <> pretty identifier <> if value then " true" else " false"
    InteractionAdvanced index -> from $ toLazyText $ "InteractionAdvanced " <> decimal index
    InteractionChoiceOpened -> "InteractionChoiceOpened"
    InteractionChoiceSelected index -> from $ toLazyText $ "InteractionChoiceSelected " <> decimal index
    InteractionEnded -> "InteractionEnded"
    InteractionStarted _ -> "InteractionStarted"
    InventoryChanged identifier count -> "InventoryChanged " <> identifier <> " " <> from (toLazyText (decimal count))
    MoveUsed side identifier _ -> "MoveUsed " <> pretty side <> " " <> pretty identifier
    NpcMoved identifier destination -> "NpcMoved " <> pretty identifier <> " " <> pretty destination
    ObjectRemoved identifier -> "ObjectRemoved " <> pretty identifier
    PartyChanged _ -> "PartyChanged"
    PartyClosed -> "PartyClosed"
    PartyOpened -> "PartyOpened"
    PlayerMoved destination -> "PlayerMoved " <> pretty destination
    PlayerTurned direction -> "PlayerTurned " <> pretty direction
    PlayerWarped destination -> "PlayerWarped " <> pretty destination
    RandomAdvanced seed -> from $ toLazyText $ "RandomAdvanced " <> decimal seed
    TurnCompleted _ -> "TurnCompleted"
