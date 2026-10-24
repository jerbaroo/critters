module Critters.CLI.Options (Options (..)) where

import Critters.InterfaceType (InterfaceType)

data Options = Options
  { gameFile :: FilePath,
    interfaceType :: InterfaceType,
    saveFile :: FilePath
  }
  deriving stock (Eq)
