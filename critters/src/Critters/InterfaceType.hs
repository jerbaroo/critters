module Critters.InterfaceType (InterfaceType (..)) where

data InterfaceType
    = {- | Game state is not drawn.
      User input is typed on a prompt line.
      Logs go to stdout and to './critters_logs'.
      -}
      Debug
    | {- | Game state is drawn in the terminal.
      User input is not visible.
      Logs go to './critters_logs'.
      -}
      TUI
    deriving stock (Eq)
