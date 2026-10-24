module Critters.Event (Event (..)) where

import Autodocodec (Autodocodec (..), HasCodec, JSONObjectCodec)
import Autodocodec qualified as Codec
import Critters.Input.Direction (Direction)
import Critters.Pretty (Pretty (..))
import Critters.State.ActiveInteraction (ActiveInteraction)
import Critters.State.Critter (Critter)
import Critters.State.Critter qualified as Critter
import Critters.State.FlagId (FlagId)
import Critters.State.Location (Location)
import Critters.State.NpcId (NpcId)
import Critters.State.ObjectId (ObjectId)
import Critters.State.Position (Position)
import Data.Aeson qualified as Aeson
import Data.HashMap.Strict qualified as HashMap
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy.Builder.Int (decimal)
import Witch (from)

-- | An established fact. Validation happens before an event is produced.
data Event
  = ConversationAdvanced Int
  | ConversationEnded
  | ConversationStarted NpcId
  | CritterReceived Critter
  | FlagChanged FlagId Bool
  | InteractionAdvanced Int
  | InteractionChoiceOpened
  | InteractionChoiceSelected Int
  | InteractionEnded
  | InteractionStarted ActiveInteraction
  | ObjectRemoved ObjectId
  | PartyClosed
  | PartyOpened
  | PlayerMoved Position
  | PlayerTurned Direction
  | PlayerWarped Location
  deriving stock (Eq)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec Event)

instance HasCodec Event where
  codec =
    Codec.object "Event"
      $ Codec.discriminatedUnionCodec "type" encode
      $ HashMap.fromList
        [ ("conversation-advanced", ("ConversationAdvanced", Codec.mapToDecoder ConversationAdvanced advanced)),
          ("conversation-ended", ("ConversationEnded", pure ConversationEnded)),
          ("conversation-started", ("ConversationStarted", Codec.mapToDecoder ConversationStarted started)),
          ("critter-received", ("CritterReceived", Codec.mapToDecoder CritterReceived given)),
          ("flag-changed", ("FlagChanged", Codec.mapToDecoder (uncurry FlagChanged) changed)),
          ("interaction-advanced", ("InteractionAdvanced", Codec.mapToDecoder InteractionAdvanced progressed)),
          ("interaction-choice-opened", ("InteractionChoiceOpened", pure InteractionChoiceOpened)),
          ("interaction-choice-selected", ("InteractionChoiceSelected", Codec.mapToDecoder InteractionChoiceSelected chosen)),
          ("interaction-ended", ("InteractionEnded", pure InteractionEnded)),
          ("interaction-started", ("InteractionStarted", Codec.mapToDecoder InteractionStarted interaction)),
          ("object-removed", ("ObjectRemoved", Codec.mapToDecoder ObjectRemoved removed)),
          ("party-closed", ("PartyClosed", pure PartyClosed)),
          ("party-opened", ("PartyOpened", pure PartyOpened)),
          ("player-moved", ("PlayerMoved", Codec.mapToDecoder PlayerMoved moved)),
          ("player-turned", ("PlayerTurned", Codec.mapToDecoder PlayerTurned turned)),
          ("player-warped", ("PlayerWarped", Codec.mapToDecoder PlayerWarped warped))
        ]
    where
      advanced :: JSONObjectCodec Int
      advanced = Codec.requiredField "lineIndex" "The new dialogue line index."
      changed :: JSONObjectCodec (FlagId, Bool)
      changed =
        (,)
          <$> Codec.requiredField "flag" "The game-defined flag identifier." Codec..= fst
          <*> Codec.requiredField "value" "The new flag value." Codec..= snd
      chosen :: JSONObjectCodec Int
      chosen = Codec.requiredField "choiceIndex" "The selected interaction choice index."
      encode = \case
        ConversationAdvanced index -> ("conversation-advanced", Codec.mapToEncoder index advanced)
        ConversationEnded -> ("conversation-ended", pure ())
        ConversationStarted identifier -> ("conversation-started", Codec.mapToEncoder identifier started)
        CritterReceived critter -> ("critter-received", Codec.mapToEncoder critter given)
        FlagChanged identifier value -> ("flag-changed", Codec.mapToEncoder (identifier, value) changed)
        InteractionAdvanced index -> ("interaction-advanced", Codec.mapToEncoder index progressed)
        InteractionChoiceOpened -> ("interaction-choice-opened", pure ())
        InteractionChoiceSelected index -> ("interaction-choice-selected", Codec.mapToEncoder index chosen)
        InteractionEnded -> ("interaction-ended", pure ())
        InteractionStarted active -> ("interaction-started", Codec.mapToEncoder active interaction)
        ObjectRemoved identifier -> ("object-removed", Codec.mapToEncoder identifier removed)
        PartyClosed -> ("party-closed", pure ())
        PartyOpened -> ("party-opened", pure ())
        PlayerMoved destination -> ("player-moved", Codec.mapToEncoder destination moved)
        PlayerTurned direction -> ("player-turned", Codec.mapToEncoder direction turned)
        PlayerWarped destination -> ("player-warped", Codec.mapToEncoder destination warped)
      given :: JSONObjectCodec Critter
      given = Codec.requiredField "critter" "The critter received by the player."
      interaction :: JSONObjectCodec ActiveInteraction
      interaction = Codec.requiredField "interaction" "The selected interaction snapshot and its initial presentation."
      moved :: JSONObjectCodec Position
      moved = Codec.requiredField "position" "The player's position after moving."
      progressed :: JSONObjectCodec Int
      progressed = Codec.requiredField "dialogueIndex" "The new scripted dialogue line index."
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
    ConversationAdvanced index -> from $ toLazyText $ "ConversationAdvanced " <> decimal index
    ConversationEnded -> "ConversationEnded"
    ConversationStarted identifier -> "ConversationStarted " <> pretty identifier
    CritterReceived critter -> "CritterReceived " <> Critter.species critter
    FlagChanged identifier value -> "FlagChanged " <> pretty identifier <> if value then " true" else " false"
    InteractionAdvanced index -> from $ toLazyText $ "InteractionAdvanced " <> decimal index
    InteractionChoiceOpened -> "InteractionChoiceOpened"
    InteractionChoiceSelected index -> from $ toLazyText $ "InteractionChoiceSelected " <> decimal index
    InteractionEnded -> "InteractionEnded"
    InteractionStarted _ -> "InteractionStarted"
    ObjectRemoved identifier -> "ObjectRemoved " <> pretty identifier
    PartyClosed -> "PartyClosed"
    PartyOpened -> "PartyOpened"
    PlayerMoved destination -> "PlayerMoved " <> pretty destination
    PlayerTurned direction -> "PlayerTurned " <> pretty direction
    PlayerWarped destination -> "PlayerWarped " <> pretty destination
