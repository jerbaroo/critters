module Critters.State.GameState (GameState (..), initialState) where

import Autodocodec (Autodocodec (..), Codec (EqCodec), HasCodec, JSONCodec)
import Autodocodec qualified as Codec
import Critters.State.Position (Position (..))
import Critters.State.Tile (Tile (..))
import Critters.State.World (World (..))
import Data.Aeson qualified as Aeson

data GameState = GameState
  { playerPosition :: Position,
    world :: World
  }
  deriving stock (Eq)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec GameState)

initialState :: GameState
initialState =
  GameState
    { playerPosition = Position 1 4,
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

versionCodec :: JSONCodec Int
versionCodec = EqCodec (1 :: Int) Codec.codec

instance HasCodec GameState where
  codec =
    Codec.object "GameState" $
      GameState
        <$> Codec.requiredField "playerPosition" "The player's position in the world." Codec..= playerPosition
        <* Codec.requiredFieldWith "version" versionCodec "Save format version. Must be 1." Codec..= const 1
        <*> Codec.requiredField "world" "The world's tile layout." Codec..= world
