module Critters.Interface (Interface (..), select) where

import Critters.Event (Event)
import Critters.Input (Input)
import Critters.Interface.Debug qualified as Debug
import Critters.Interface.TUI qualified as TUI
import Critters.Interface.TUI.Keyboard qualified as Keyboard
import Critters.InterfaceType (InterfaceType (Debug, TUI))
import Critters.State.GameState (GameState)
import Data.Text (Text)
import Prelude (FilePath, IO, pure, ($))

data Interface a = Interface
    { input :: FilePath -> IO Input
    , log :: a -> FilePath -> Text -> IO ()
    , output :: a -> GameState -> IO a
    , outputEvent :: a -> Event -> GameState -> IO a
    }

select :: InterfaceType -> (forall a. Interface a -> a -> IO result) -> IO result
select Debug use =
    use
        Interface
            { input = Debug.input
            , log = Debug.writeLog
            , output = Debug.output
            , outputEvent = \context _ _ -> pure context
            }
        ()
select TUI use =
    Keyboard.withTerminal $
        use
            Interface
                { input = TUI.input
                , log = TUI.writeLog
                , output = TUI.output
                , outputEvent = TUI.outputEvent
                }
            TUI.initialContext
