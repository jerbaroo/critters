module Critters.CLI.Options (Options (..)) where

data Options = Options
  { gameFile :: FilePath,
    saveFile :: FilePath
  }
  deriving stock (Eq)
