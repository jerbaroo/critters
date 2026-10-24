module Critters.State.Critter (Critter (..)) where

import Autodocodec (Autodocodec (..), HasCodec)
import Autodocodec qualified as Codec
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Data.Text qualified as Text

-- | An individual critter. Species identifiers are chosen by the game author.
data Critter = Critter
  { hitPoints :: Int,
    level :: Int,
    maxHitPoints :: Int,
    species :: Text
  }
  deriving stock (Eq)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec Critter)

instance HasCodec Critter where
  codec =
    Codec.bimapCodec validate id
      $ Codec.object "Critter"
      $ Critter
        <$> Codec.requiredField "hitPoints" "Current HP, from zero to maxHitPoints inclusive." Codec..= hitPoints
        <*> Codec.requiredField "level" "The critter's positive level." Codec..= level
        <*> Codec.requiredField "maxHitPoints" "The critter's positive maximum HP." Codec..= maxHitPoints
        <*> Codec.requiredField "species" "A nonblank species identifier chosen by the game author." Codec..= species
    where
      validate critter
        | hitPoints critter < 0 || hitPoints critter > maxHitPoints critter = Left "hitPoints must be between zero and maxHitPoints."
        | level critter <= 0 = Left "level must be positive."
        | maxHitPoints critter <= 0 = Left "maxHitPoints must be positive."
        | Text.null (Text.strip (species critter)) = Left "species must not be blank."
        | otherwise = Right critter
