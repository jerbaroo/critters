module Critters.State.GameState (conversationText, GameState (..), initialState, npcAt, world) where

import Autodocodec (Autodocodec (..), Codec (EqCodec), HasCodec, JSONCodec)
import Autodocodec qualified as Codec
import Critters.Input.Direction (Direction (North))
import Critters.State.Conversation (Conversation (..))
import Critters.State.Location (Location (..))
import Critters.State.MapId (MapId (..))
import Critters.State.Npc (Npc)
import Critters.State.Npc qualified as Npc
import Critters.State.NpcId (NpcId)
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
  { conversation :: Maybe Conversation,
    maps :: Map MapId World,
    npcs :: Map NpcId Npc,
    playerDirection :: Direction,
    playerMap :: MapId,
    playerPosition :: Position
  }
  deriving stock (Eq)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec GameState)

initialState :: GameState
initialState =
  GameState
    { conversation = Nothing,
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
      npcs = Map.empty,
      playerDirection = North,
      playerMap = MapId "overworld",
      playerPosition = Position 1 4
    }

npcAt :: GameState -> Position -> Maybe (NpcId, Npc)
npcAt state coordinates =
  find ((== Location (playerMap state) coordinates) . Npc.location . snd) (Map.toAscList (npcs state))

versionCodec :: JSONCodec Int
versionCodec = EqCodec (1 :: Int) Codec.codec

-- | Select the active tile map. An unknown map has no tiles to render or enter.
world :: GameState -> World
world state = Map.findWithDefault (World [] []) (playerMap state) (maps state)

instance HasCodec GameState where
  codec = Codec.object "GameState" $ Codec.parseAlternative current legacy
    where
      current =
        GameState
          <$> Codec.optionalFieldOrNull "conversation" "The active conversation, if any." Codec..= conversation
          <*> Codec.requiredField "maps" "Tile maps indexed by their stable identifiers." Codec..= maps
          <*> Codec.optionalFieldWithOmittedDefault "npcs" Map.empty "Stationary NPCs indexed by their stable identifiers." Codec..= npcs
          <*> Codec.optionalFieldWithOmittedDefault "playerDirection" North "The direction the player faces." Codec..= playerDirection
          <*> Codec.optionalFieldWithOmittedDefault "playerMap" (MapId "overworld") "The active map." Codec..= playerMap
          <*> Codec.requiredField "playerPosition" "The player's coordinates in the active map." Codec..= playerPosition
          <* Codec.requiredFieldWith "version" versionCodec "Save format version. Must be 1." Codec..= const 1
      legacy =
        ( \active actors facing coordinates layout ->
            GameState active (Map.singleton (MapId "overworld") layout) actors facing (MapId "overworld") coordinates
        )
          <$> Codec.optionalFieldOrNull "conversation" "The active conversation, if any." Codec..= conversation
          <*> Codec.optionalFieldWithOmittedDefault "npcs" Map.empty "Stationary NPCs." Codec..= npcs
          <*> Codec.optionalFieldWithOmittedDefault "playerDirection" North "The direction the player faces." Codec..= playerDirection
          <*> Codec.requiredField "playerPosition" "Coordinates in the overworld." Codec..= playerPosition
          <*> Codec.requiredField "world" "Legacy single-map layout." Codec..= world
          <* Codec.requiredFieldWith "version" versionCodec "Save format version. Must be 1." Codec..= const 1
