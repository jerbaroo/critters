module Critters.Pretty (Pretty (..)) where

import Data.Text (Text)

-- | Human-readable rendering.
class Pretty a where
  pretty :: a -> Text
