-- | Serializable game data, independent of game rules and rendering.
module Critters.State
  ( module Critters.State.ActiveInteraction,
    module Critters.State.Affliction,
    module Critters.State.BattleProfile,
    module Critters.State.Choice,
    module Critters.State.Condition,
    module Critters.State.Conversation,
    module Critters.State.Critter,
    module Critters.State.Effect,
    module Critters.State.FlagId,
    module Critters.State.GameState,
    module Critters.State.Interaction,
    module Critters.State.Location,
    module Critters.State.MapId,
    module Critters.State.Npc,
    module Critters.State.NpcId,
    module Critters.State.Object,
    module Critters.State.ObjectId,
    module Critters.State.Party,
    module Critters.State.Position,
    module Critters.State.Species,
    module Critters.State.Tile,
    module Critters.State.Warp,
    module Critters.State.World,
  )
where

import Critters.State.ActiveInteraction (ActiveInteraction (..))
import Critters.State.Affliction (Affliction (..))
import Critters.State.BattleProfile (BattleProfile (..), defaultProfile)
import Critters.State.Choice (Choice (..))
import Critters.State.Condition (Condition (..))
import Critters.State.Conversation (Conversation (..))
import Critters.State.Critter (Critter (..), basicCritter)
import Critters.State.Effect (Effect (..))
import Critters.State.FlagId (FlagId (..))
import Critters.State.GameState (GameState (..), conversationText, initialState, interactionText, npcAt, objectAt, world)
import Critters.State.Interaction (Interaction (..))
import Critters.State.Location (Location (..))
import Critters.State.MapId (MapId (..))
import Critters.State.Npc (Npc (..))
import Critters.State.NpcId (NpcId (..))
import Critters.State.Object (Object (..))
import Critters.State.ObjectId (ObjectId (..))
import Critters.State.Party (Party (..), empty)
import Critters.State.Position (Position (..))
import Critters.State.Species (Species (..), defaultSpecies)
import Critters.State.Tile (Tile (..))
import Critters.State.Warp (Warp (..))
import Critters.State.World (World (..), tileAt)
