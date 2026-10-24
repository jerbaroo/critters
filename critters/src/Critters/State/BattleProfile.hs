module Critters.State.BattleProfile (BattleProfile (..), defaultProfile) where

import Autodocodec qualified as Codec
import Autodocodec (Autodocodec (..), HasCodec)
import Critters.State.Affliction (Affliction)
import Critters.State.MoveId (MoveId)
import Data.Aeson qualified as Aeson
import Data.List (nub)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

data BattleProfile = BattleProfile
  { profileExperience :: Int,
    profileKnownMoves :: [MoveId],
    profileRemainingUses :: Map MoveId Int,
    profileStatus :: Maybe Affliction
  }
  deriving stock (Eq)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec BattleProfile)

defaultProfile :: BattleProfile
defaultProfile = BattleProfile
  { profileExperience = 0,
    profileKnownMoves = [],
    profileRemainingUses = Map.empty,
    profileStatus = Nothing
  }

instance HasCodec BattleProfile where
  codec = Codec.bimapCodec validate id $ Codec.object "BattleProfile" $ BattleProfile
    <$> Codec.optionalFieldWithOmittedDefault "experience" (0) "Total experience." Codec..= profileExperience
    <*> Codec.optionalFieldWithOmittedDefault "knownMoves" ([]) "Individually learned moves; empty derives the species learnset." Codec..= profileKnownMoves
    <*> Codec.optionalFieldWithOmittedDefault "remainingUses" (Map.empty) "Remaining uses; absent entries are fully restored." Codec..= profileRemainingUses
    <*> Codec.optionalFieldOrNull "status" "Persistent status." Codec..= profileStatus
    where
      validate value
        | profileExperience value < 0 || any (< 0) (Map.elems (profileRemainingUses value)) = Left "Experience and remaining uses must not be negative."
        | length (nub (profileKnownMoves value)) /= length (profileKnownMoves value) = Left "Known moves must be unique."
        | otherwise = Right value
