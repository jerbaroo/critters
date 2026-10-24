-- | Serializable game data, independent of game rules and rendering.
module Critters.State
  ( module Critters.State.Conversation,
    module Critters.State.GameState,
    module Critters.State.Npc,
    module Critters.State.NpcId,
    module Critters.State.Position,
    module Critters.State.Tile,
    module Critters.State.World,
  )
where

import Critters.State.Conversation (Conversation (..))
import Critters.State.GameState (GameState (..), conversationText, initialState, npcAt)
import Critters.State.Npc (Npc (..))
import Critters.State.NpcId (NpcId (..))
import Critters.State.Position (Position (..))
import Critters.State.Tile (Tile (..))
import Critters.State.World (World (..), tileAt)
