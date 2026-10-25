-- | Serializable game data, independent of game rules and rendering.
module Critters.State
  ( module Critters.State.ActiveInteraction,
    module Critters.State.Affliction,
    module Critters.State.Battle,
    module Critters.State.BattleDefinition,
    module Critters.State.BattleOutcome,
    module Critters.State.BattlePhase,
    module Critters.State.BattleProfile,
    module Critters.State.BattleSide,
    module Critters.State.BattleUpdate,
    module Critters.State.Choice,
    module Critters.State.CombatConfig,
    module Critters.State.Condition,
    module Critters.State.Conversation,
    module Critters.State.Critter,
    module Critters.State.Effect,
    module Critters.State.Encounter,
    module Critters.State.EncounterEntry,
    module Critters.State.FlagId,
    module Critters.State.GameState,
    module Critters.State.Interaction,
    module Critters.State.Item,
    module Critters.State.Location,
    module Critters.State.MapId,
    module Critters.State.Move,
    module Critters.State.MoveEffect,
    module Critters.State.MoveId,
    module Critters.State.Npc,
    module Critters.State.NpcId,
    module Critters.State.Object,
    module Critters.State.ObjectId,
    module Critters.State.Party,
    module Critters.State.Position,
    module Critters.State.Rules,
    module Critters.State.Species,
    module Critters.State.StatusDefinition,
    module Critters.State.Tile,
    module Critters.State.Warp,
    module Critters.State.World,
  )
where

import Critters.State.ActiveInteraction (ActiveInteraction (..))
import Critters.State.Affliction (Affliction (..))
import Critters.State.Battle (Battle (..), defaultBattle)
import Critters.State.BattleDefinition (BattleDefinition (..), defaultDefinition)
import Critters.State.BattleOutcome (BattleOutcome (..))
import Critters.State.BattlePhase (BattlePhase (..))
import Critters.State.BattleProfile (BattleProfile (..), defaultProfile)
import Critters.State.BattleSide (BattleSide (..))
import Critters.State.BattleUpdate (BattleUpdate (..))
import Critters.State.Choice (Choice (..))
import Critters.State.CombatConfig (CombatConfig (..), defaultCombat)
import Critters.State.Condition (Condition (..))
import Critters.State.Conversation (Conversation (..))
import Critters.State.Critter (Critter (..), basicCritter)
import Critters.State.Effect (Effect (..))
import Critters.State.Encounter (Encounter (..))
import Critters.State.EncounterEntry (EncounterEntry (..))
import Critters.State.FlagId (FlagId (..))
import Critters.State.GameState (GameState (..), conversationText, initialState, interactionText, npcAt, objectAt, world)
import Critters.State.Interaction (Interaction (..))
import Critters.State.Item (Item (..), defaultItem)
import Critters.State.Location (Location (..))
import Critters.State.MapId (MapId (..))
import Critters.State.Move (Move (..), defaultMove)
import Critters.State.MoveEffect (MoveEffect (..), Target (..))
import Critters.State.MoveId (MoveId (..))
import Critters.State.Npc (Npc (..))
import Critters.State.NpcId (NpcId (..))
import Critters.State.Object (Object (..))
import Critters.State.ObjectId (ObjectId (..))
import Critters.State.Party (Party (..), empty)
import Critters.State.Position (Position (..))
import Critters.State.Rules (Rules (..), defaultRules)
import Critters.State.Species (Species (..), defaultSpecies)
import Critters.State.StatusDefinition (StatusDefinition (..), defaultStatus)
import Critters.State.Tile (Tile (..))
import Critters.State.Warp (Warp (..))
import Critters.State.World (World (..), tileAt)
