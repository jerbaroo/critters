-- | Serializable game data, independent of game rules and rendering.
module Critters.State
  ( module Critters.State.GameState,
    module Critters.State.Position,
    module Critters.State.Tile,
    module Critters.State.World,
  )
where

import Critters.State.GameState (GameState (..), initialState)
import Critters.State.Position (Position (..))
import Critters.State.Tile (Tile (..))
import Critters.State.World (World (..), tileAt)
