module Critters.Interface (Interface (..), select) where

import Critters.Input (Input)
import Critters.Interface.Debug qualified as Debug
import Critters.InterfaceType (InterfaceType (Debug))
import Critters.State.GameState (GameState)
import Data.Text (Text)
import Prelude (IO)

data Interface = Interface
  { input :: IO Input,
    log :: Text -> IO (),
    output :: GameState -> IO ()
  }

select :: InterfaceType -> Interface
select Debug =
  Interface
    { input = Debug.input,
      log = Debug.writeLog,
      output = Debug.output
    }
