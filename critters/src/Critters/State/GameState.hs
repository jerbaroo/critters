module Critters.State.GameState (conversationText, GameState (..), initialState, npcAt) where

import Autodocodec (Autodocodec (..), Codec (EqCodec), HasCodec, JSONCodec)
import Autodocodec qualified as Codec
import Critters.Input.Direction (Direction (North))
import Critters.State.Conversation (Conversation (..))
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
    line : _ | lineIndex active >= 0 -> Just line
    _ -> Nothing

data GameState = GameState
  { conversation :: Maybe Conversation,
    npcs :: Map NpcId Npc,
    playerDirection :: Direction,
    playerPosition :: Position,
    world :: World
  }
  deriving stock (Eq)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec GameState)

initialState :: GameState
initialState =
  GameState
    { conversation = Nothing,
      npcs = Map.empty,
      playerDirection = North,
      playerPosition = Position 1 4,
      world =
        World $
          map
            (map Just)
            [ replicate 8 Wall,
              [Wall, Ground, Ground, Ground, Ground, TallGrass, TallGrass, Wall],
              [Wall, Ground, Ground, Ground, Ground, TallGrass, TallGrass, Wall],
              [Wall, Ground, Water, Water, Ground, Ground, Ground, Wall],
              [Wall, Ground, Water, Water, Ground, Ground, Ground, Wall],
              replicate 8 Wall
            ]
    }

npcAt :: GameState -> Position -> Maybe (NpcId, Npc)
npcAt state location = find ((== location) . Npc.position . snd) (Map.toAscList (npcs state))

versionCodec :: JSONCodec Int
versionCodec = EqCodec (1 :: Int) Codec.codec

instance HasCodec GameState where
  codec =
    Codec.object "GameState" $
      GameState
        <$> Codec.optionalFieldOrNull "conversation" "The active conversation, if any." Codec..= conversation
        <*> Codec.optionalFieldWithOmittedDefault "npcs" Map.empty "Stationary NPCs indexed by their stable identifiers." Codec..= npcs
        <*> Codec.optionalFieldWithOmittedDefault "playerDirection" North "The direction the player faces." Codec..= playerDirection
        <*> Codec.requiredField "playerPosition" "The player's position in the world." Codec..= playerPosition
        <* Codec.requiredFieldWith "version" versionCodec "Save format version. Must be 1." Codec..= const 1
        <*> Codec.requiredField "world" "The world's tile layout." Codec..= world
