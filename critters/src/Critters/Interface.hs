module Critters.Interface (Interface (..), select) where

import Critters.Input (Input)
import Critters.Interface.Debug qualified as Debug
import Critters.Interface.TUI qualified as TUI
import Critters.Interface.TUI.Keyboard qualified as Keyboard
import Critters.InterfaceType (InterfaceType (Debug, TUI))
import Critters.State.GameState (GameState)
import Data.Text (Text)
import Prelude (FilePath, IO, ($))

data Interface a = Interface
  { input :: FilePath -> IO Input,
    log :: a -> FilePath -> Text -> IO (),
    output :: GameState -> IO a
  }

select :: InterfaceType -> (Interface () -> IO result) -> IO result
select Debug use =
  use
    Interface
      { input = Debug.input,
        log = Debug.writeLog,
        output = Debug.output
      }
select TUI use =
  Keyboard.withTerminal $
    use
      Interface
        { input = TUI.input,
          log = TUI.writeLog,
          output = TUI.output
        }
