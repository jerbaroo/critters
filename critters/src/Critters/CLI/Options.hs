module Critters.CLI.Options (Options (..)) where

import Critters.InterfaceType (InterfaceType)
import Data.Text (Text)

data Options = Options
    { gameFile :: FilePath
    , interfaceType :: InterfaceType
    , saveName :: Text
    }
    deriving stock (Eq)
