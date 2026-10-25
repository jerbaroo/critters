module Critters.State.Critter (basicCritter, Critter (..)) where

import Autodocodec (Autodocodec (..), HasCodec)
import Autodocodec qualified as Codec
import Critters.State.BattleProfile (BattleProfile, defaultProfile)
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Data.Text qualified as Text

-- | An individual critter. Species identifiers are chosen by the game author.
basicCritter :: Int -> Int -> Int -> Text -> Critter
basicCritter = Critter defaultProfile

data Critter = Critter
    { battleProfile :: BattleProfile
    , hitPoints :: Int
    , level :: Int
    , maxHitPoints :: Int
    , species :: Text
    }
    deriving stock (Eq)
    deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec Critter)

instance HasCodec Critter where
    codec =
        Codec.named "Critter" $
            Codec.bimapCodec validate id $
                Codec.object "Critter" $
                    Critter
                        <$> Codec.optionalFieldWithOmittedDefault "battle" defaultProfile "Persistent battle data." Codec..= (.battleProfile)
                        <*> Codec.requiredField "hitPoints" "Current HP, from zero to maxHitPoints inclusive." Codec..= (.hitPoints)
                        <*> Codec.requiredField "level" "The critter's positive level." Codec..= (.level)
                        <*> Codec.requiredField "maxHitPoints" "The critter's positive maximum HP." Codec..= (.maxHitPoints)
                        <*> Codec.requiredField "species" "A nonblank species identifier chosen by the game author." Codec..= (.species)
      where
        validate critter
            | critter.hitPoints < 0 || critter.hitPoints > critter.maxHitPoints = Left "hitPoints must be between zero and maxHitPoints."
            | critter.level <= 0 = Left "level must be positive."
            | critter.maxHitPoints <= 0 = Left "maxHitPoints must be positive."
            | Text.null (Text.strip critter.species) = Left "species must not be blank."
            | otherwise = Right critter
