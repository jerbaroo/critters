module Critters.Event (Event (..)) where

import Autodocodec (Autodocodec (..), HasCodec, JSONObjectCodec)
import Autodocodec qualified as Codec
import Critters.Input.Direction (Direction)
import Critters.Pretty (Pretty (..))
import Critters.State.Location (Location)
import Critters.State.NpcId (NpcId)
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
          ("player-moved", ("PlayerMoved", Codec.mapToDecoder PlayerMoved moved)),
          ("player-turned", ("PlayerTurned", Codec.mapToDecoder PlayerTurned turned)),
          ("player-warped", ("PlayerWarped", Codec.mapToDecoder PlayerWarped warped))
        ]
    where
      advanced :: JSONObjectCodec Int
      advanced = Codec.requiredField "lineIndex" "The new dialogue line index."
      encode = \case
        ConversationAdvanced index -> ("conversation-advanced", Codec.mapToEncoder index advanced)
        ConversationEnded -> ("conversation-ended", pure ())
        ConversationStarted identifier -> ("conversation-started", Codec.mapToEncoder identifier started)
        PlayerMoved destination -> ("player-moved", Codec.mapToEncoder destination moved)
        PlayerTurned direction -> ("player-turned", Codec.mapToEncoder direction turned)
        PlayerWarped destination -> ("player-warped", Codec.mapToEncoder destination warped)
      moved :: JSONObjectCodec Position
      moved = Codec.requiredField "position" "The player's position after moving."
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
    PlayerMoved destination -> "PlayerMoved " <> pretty destination
    PlayerTurned direction -> "PlayerTurned " <> pretty direction
    PlayerWarped destination -> "PlayerWarped " <> pretty destination
