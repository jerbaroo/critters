-- | Serializable game data, independent of game rules and rendering.
module Critters.State
  ( module Critters.State.Conversation,
    module Critters.State.Critter,
    module Critters.State.GameState,
    module Critters.State.Location,
    module Critters.State.MapId,
    module Critters.State.Npc,
    module Critters.State.NpcId,
    module Critters.State.Party,
    module Critters.State.Position,
    module Critters.State.Tile,
    module Critters.State.Warp,
    module Critters.State.World,
  )
where

import Critters.State.Conversation (Conversation (..))
import Critters.State.Critter (Critter (..))
import Critters.State.GameState (GameState (..), conversationText, initialState, npcAt, world)
import Critters.State.Location (Location (..))
import Critters.State.MapId (MapId (..))
import Critters.State.Npc (Npc (..))
import Critters.State.NpcId (NpcId (..))
import Critters.State.Party (Party (..), empty)
import Critters.State.Position (Position (..))
import Critters.State.Tile (Tile (..))
import Critters.State.Warp (Warp (..))
import Critters.State.World (World (..), tileAt)
