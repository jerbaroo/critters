module Critters.State.GameState (conversationText, GameState (..), initialState, interactionText, npcAt, objectAt, world) where

import Autodocodec (Autodocodec (..), Codec (EqCodec), HasCodec, JSONCodec)
import Autodocodec qualified as Codec
import Critters.Input.Direction (Direction (North))
import Critters.State.ActiveInteraction (ActiveInteraction)
import Critters.State.ActiveInteraction qualified as Active
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
    conversation :: Maybe Conversation,
    flags :: Map FlagId Bool,
    maps :: Map MapId World,
    maxPartySize :: Int,
    npcs :: Map NpcId Npc,
    objects :: Map ObjectId Object,
    party :: Party,
    partyOpen :: Bool,
    playerDirection :: Direction,
    playerMap :: MapId,
    playerPosition :: Position
  }
  deriving stock (Eq)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec GameState)

initialState :: GameState
initialState =
  GameState
    { activeInteraction = Nothing,
      conversation = Nothing,
      flags = Map.empty,
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
      playerPosition = Position 1 4
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
      current =
        GameState
          <$> Codec.optionalFieldOrNull "activeInteraction" "The selected scripted interaction and its current presentation." Codec..= activeInteraction
          <*> Codec.optionalFieldOrNull "conversation" "The active legacy NPC conversation, if any." Codec..= conversation
          <*> Codec.optionalFieldWithOmittedDefault "flags" Map.empty "Named story flags; missing flags are false." Codec..= flags
          <*> Codec.requiredField "maps" "Tile maps indexed by their stable identifiers." Codec..= maps
          <*> Codec.optionalFieldWithOmittedDefault "maxPartySize" 6 "The positive game-wide limit on party members." Codec..= maxPartySize
          <*> Codec.optionalFieldWithOmittedDefault "npcs" Map.empty "Stationary NPCs indexed by their stable identifiers." Codec..= npcs
          <*> Codec.optionalFieldWithOmittedDefault "objects" Map.empty "Interactable world objects indexed by stable identifiers." Codec..= objects
          <*> Codec.optionalFieldWithOmittedDefault "party" Party.empty "The player's ordered party." Codec..= party
          <*> Codec.optionalFieldWithOmittedDefault "partyOpen" False "Whether the party view is open." Codec..= partyOpen
          <*> Codec.optionalFieldWithOmittedDefault "playerDirection" North "The direction the player faces." Codec..= playerDirection
          <*> Codec.optionalFieldWithOmittedDefault "playerMap" (MapId "overworld") "The active map." Codec..= playerMap
          <*> Codec.requiredField "playerPosition" "The player's coordinates in the active map." Codec..= playerPosition
          <* Codec.requiredFieldWith "version" versionCodec "Save format version. Must be 1." Codec..= const 1
      legacy =
        ( \script active storyFlags limit actors props critters opened facing coordinates layout ->
            GameState script active storyFlags (Map.singleton (MapId "overworld") layout) limit actors props critters opened facing (MapId "overworld") coordinates
        )
          <$> Codec.optionalFieldOrNull "activeInteraction" "The selected scripted interaction and its current presentation." Codec..= activeInteraction
          <*> Codec.optionalFieldOrNull "conversation" "The active legacy NPC conversation, if any." Codec..= conversation
          <*> Codec.optionalFieldWithOmittedDefault "flags" Map.empty "Named story flags; missing flags are false." Codec..= flags
          <*> Codec.optionalFieldWithOmittedDefault "maxPartySize" 6 "The positive game-wide limit on party members." Codec..= maxPartySize
          <*> Codec.optionalFieldWithOmittedDefault "npcs" Map.empty "Stationary NPCs." Codec..= npcs
          <*> Codec.optionalFieldWithOmittedDefault "objects" Map.empty "Interactable world objects indexed by stable identifiers." Codec..= objects
          <*> Codec.optionalFieldWithOmittedDefault "party" Party.empty "The player's ordered party." Codec..= party
          <*> Codec.optionalFieldWithOmittedDefault "partyOpen" False "Whether the party view is open." Codec..= partyOpen
          <*> Codec.optionalFieldWithOmittedDefault "playerDirection" North "The direction the player faces." Codec..= playerDirection
          <*> Codec.requiredField "playerPosition" "Coordinates in the overworld." Codec..= playerPosition
          <*> Codec.requiredField "world" "Legacy single-map layout." Codec..= world
          <* Codec.requiredFieldWith "version" versionCodec "Save format version. Must be 1." Codec..= const 1
      validate state
        | maxPartySize state <= 0 = Left "maxPartySize must be positive."
        | length (Party.members (party state)) > maxPartySize state = Left "Party members exceed the game's maxPartySize."
        | otherwise = Right state
