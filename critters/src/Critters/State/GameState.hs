module Critters.State.GameState (conversationText, GameState (..), initialState, interactionText, npcAt, objectAt, world) where

import Autodocodec (Autodocodec (..), Codec (EqCodec), HasCodec, JSONCodec, JSONObjectCodec)
import Autodocodec qualified as Codec
import Critters.Input.Direction (Direction (North))
import Critters.State.ActiveInteraction (ActiveInteraction)
import Critters.State.ActiveInteraction qualified as Active
import Critters.State.Battle (Battle)
import Critters.State.Battle qualified as Battle
import Critters.State.CombatConfig (CombatConfig, defaultCombat)
import Critters.State.CombatConfig qualified as Combat
import Critters.State.Encounter qualified as Encounter
import Critters.State.Conversation (Conversation (..))
import Critters.State.FlagId (FlagId)
import Critters.State.Interaction qualified as Interaction
import Critters.State.Location (Location (..))
import Critters.State.MapId (MapId (..))
import Critters.State.Npc (Npc)
import Critters.State.Npc qualified as Npc
import Critters.State.NpcId (NpcId)
import Critters.State.Object (Object)
import Critters.State.Object qualified as Object
import Critters.State.ObjectId (ObjectId)
import Critters.State.Party (Party)
import Critters.State.Party qualified as Party
import Critters.State.Position (Position (..))
import Critters.State.Tile (Tile (..))
import Critters.State.World (World (..))
import Data.Aeson qualified as Aeson
import Data.List (find)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)

conversationText :: GameState -> Maybe Text
conversationText state = do
  active <- conversation state
  npc <- Map.lookup (npcId active) (npcs state)
  case drop (lineIndex active) (Npc.dialogue npc) of
    line : _ | lineIndex active >= 0 && mapId (Npc.location npc) == playerMap state -> Just line
    _ -> Nothing

data GameState = GameState
  { activeInteraction :: Maybe ActiveInteraction,
    battle :: Maybe Battle,
    combat :: CombatConfig,
    conversation :: Maybe Conversation,
    encounterSteps :: Int,
    flags :: Map FlagId Bool,
    inventory :: Map Text Int,
    maps :: Map MapId World,
    maxPartySize :: Int,
    npcs :: Map NpcId Npc,
    objects :: Map ObjectId Object,
    party :: Party,
    partyOpen :: Bool,
    playerDirection :: Direction,
    playerMap :: MapId,
    playerPosition :: Position,
    randomSeed :: Int
  }
  deriving stock (Eq)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec GameState)

initialState :: GameState
initialState =
  GameState
    { activeInteraction = Nothing,
      battle = Nothing,
      combat = defaultCombat,
      conversation = Nothing,
      encounterSteps = 0,
      flags = Map.empty,
      inventory = Map.empty,
      maps =
        Map.singleton (MapId "overworld") $
          World
            { tiles =
                map
                  (map Just)
                  [ replicate 8 Wall,
                    [Wall, Ground, Ground, Ground, Ground, TallGrass, TallGrass, Wall],
                    [Wall, Ground, Ground, Ground, Ground, TallGrass, TallGrass, Wall],
                    [Wall, Ground, Water, Water, Ground, Ground, Ground, Wall],
                    [Wall, Ground, Water, Water, Ground, Ground, Ground, Wall],
                    replicate 8 Wall
                  ],
              warps = []
            },
      maxPartySize = 6,
      npcs = Map.empty,
      objects = Map.empty,
      party = Party.empty,
      partyOpen = False,
      playerDirection = North,
      playerMap = MapId "overworld",
      playerPosition = Position 1 4,
      randomSeed = 1
    }

-- | Read from the interaction snapshot, without choosing a branch again.
interactionText :: GameState -> Maybe Text
interactionText state = do
  active <- activeInteraction state
  let index = Active.activeDialogueIndex active
  case drop index (Interaction.interactionDialogue (Active.activeScript active)) of
    line : _ | index >= 0 -> Just line
    _ -> Nothing

npcAt :: GameState -> Position -> Maybe (NpcId, Npc)
npcAt state coordinates =
  find ((== Location (playerMap state) coordinates) . Npc.location . snd) (Map.toAscList (npcs state))

objectAt :: GameState -> Position -> Maybe (ObjectId, Object)
objectAt state coordinates =
  find ((== Location (playerMap state) coordinates) . Object.objectLocation . snd) (Map.toAscList (objects state))

versionCodec :: JSONCodec Int
versionCodec = EqCodec (1 :: Int) Codec.codec

-- | Select the active tile map. An unknown map has no tiles to render or enter.
world :: GameState -> World
world state = Map.findWithDefault (World [] []) (playerMap state) (maps state)

instance HasCodec GameState where
  codec = Codec.bimapCodec validate id $ Codec.object "GameState" $ Codec.parseAlternative current legacy
    where
      current = fields (Codec.requiredField "maps" "Tile maps indexed by stable identifiers.") (Codec.optionalFieldWithOmittedDefault "playerMap" (MapId "overworld") "The active map.")
      fields :: JSONObjectCodec (Map MapId World) -> JSONObjectCodec MapId -> JSONObjectCodec GameState
      fields layouts activeMap = GameState
        <$> Codec.optionalFieldOrNull "activeInteraction" "Selected scripted interaction." Codec..= activeInteraction
        <*> Codec.optionalFieldOrNull "battle" "Active battle, including its configuration snapshot." Codec..= battle
        <*> Codec.optionalFieldWithOmittedDefault "combat" defaultCombat "Game-defined battle rules and content." Codec..= combat
        <*> Codec.optionalFieldOrNull "conversation" "Active legacy conversation." Codec..= conversation
        <*> Codec.optionalFieldWithOmittedDefault "encounterSteps" 0 "Eligible steps since the previous encounter." Codec..= encounterSteps
        <*> Codec.optionalFieldWithOmittedDefault "flags" Map.empty "Named story flags; missing flags are false." Codec..= flags
        <*> Codec.optionalFieldWithOmittedDefault "inventory" Map.empty "Item counts indexed by game-defined identifiers." Codec..= inventory
        <*> layouts Codec..= maps
        <*> Codec.optionalFieldWithOmittedDefault "maxPartySize" 6 "Positive game-wide party limit." Codec..= maxPartySize
        <*> Codec.optionalFieldWithOmittedDefault "npcs" Map.empty "Stationary NPCs." Codec..= npcs
        <*> Codec.optionalFieldWithOmittedDefault "objects" Map.empty "Interactable world objects." Codec..= objects
        <*> Codec.optionalFieldWithOmittedDefault "party" Party.empty "Player party." Codec..= party
        <*> Codec.optionalFieldWithOmittedDefault "partyOpen" False "Whether the party view is open." Codec..= partyOpen
        <*> Codec.optionalFieldWithOmittedDefault "playerDirection" North "Player facing direction." Codec..= playerDirection
        <*> activeMap Codec..= playerMap
        <*> Codec.requiredField "playerPosition" "Coordinates in the active map." Codec..= playerPosition
        <*> Codec.optionalFieldWithOmittedDefault "randomSeed" 1 "Reproducible random state, from zero through 2147483647." Codec..= randomSeed
        <* Codec.requiredFieldWith "version" versionCodec "Save format version." Codec..= const 1
      legacy = fields (Codec.dimapCodec (Map.singleton (MapId "overworld")) (Map.findWithDefault (World [] []) (MapId "overworld")) (Codec.requiredField "world" "Legacy single map.")) (Codec.pureCodec (MapId "overworld"))
      validate state
        | maxPartySize state <= 0 = Left "maxPartySize must be positive."
        | length (Party.members (party state)) > maxPartySize state = Left "Party members exceed the game party limit."
        | randomSeed state < 0 || randomSeed state > 2147483647 || encounterSteps state < 0 = Left "Invalid random seed or encounter step count."
        | any (< 0) (Map.elems (inventory state)) = Left "Inventory counts must not be negative."
        | any (\table -> not (Map.member (Encounter.encounterMapId table) (maps state))) (Combat.configEncounters (combat state)) = Left "An encounter references a missing map."
        | maybe False (\active -> Battle.battleActivePlayer active >= length (Party.members (party state)) || any (>= length (Party.members (party state))) (Battle.battleParticipants active)) (battle state) = Left "Active battle references a missing party member."
        | otherwise = Right state
