module Critters.State.BattleProfile (BattleProfile (..), defaultProfile) where

import Autodocodec (Autodocodec (..), HasCodec)
import Autodocodec qualified as Codec
import Critters.State.Affliction (Affliction)
import Critters.State.MoveId (MoveId)
import Data.Aeson qualified as Aeson
import Data.List (nub)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

data BattleProfile = BattleProfile
    { experience :: Int
    , knownMoves :: [MoveId]
    , remainingUses :: Map MoveId Int
    , status :: Maybe Affliction
    }
    deriving stock (Eq)
    deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec BattleProfile)

defaultProfile :: BattleProfile
defaultProfile =
    BattleProfile
        { experience = 0
        , knownMoves = []
        , remainingUses = Map.empty
        , status = Nothing
        }

instance HasCodec BattleProfile where
    codec =
        Codec.bimapCodec validate id $
            Codec.object "BattleProfile" $
                BattleProfile
                    <$> Codec.optionalFieldWithOmittedDefault "experience" (0) "Total experience." Codec..= (.experience)
                    <*> Codec.optionalFieldWithOmittedDefault "knownMoves" ([]) "Individually learned moves; empty derives the species learnset." Codec..= (.knownMoves)
                    <*> Codec.optionalFieldWithOmittedDefault "remainingUses" (Map.empty) "Remaining uses; absent entries are fully restored." Codec..= (.remainingUses)
                    <*> Codec.optionalFieldOrNull "status" "Persistent status." Codec..= (.status)
      where
        validate value
            | value.experience < 0 || any (< 0) (Map.elems value.remainingUses) = Left "Experience and remaining uses must not be negative."
            | length (nub value.knownMoves) /= length value.knownMoves = Left "Known moves must be unique."
            | otherwise = Right value
