module Critters.State.BattleUpdate (BattleUpdate (..)) where

import Autodocodec (Autodocodec (..), HasCodec)
import Autodocodec qualified as Codec
import Critters.State.Battle (Battle)
import Critters.State.Party (Party)
import Data.Aeson qualified as Aeson
import Data.Map.Strict (Map)
import Data.Text (Text)

data BattleUpdate = BattleUpdate
    { battle :: Battle
    , inventory :: Maybe (Map Text Int)
    , party :: Maybe Party
    , seed :: Maybe Int
    }
    deriving stock (Eq)
    deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec BattleUpdate)

instance HasCodec BattleUpdate where
    codec =
        Codec.object "BattleUpdate" $
            BattleUpdate
                <$> Codec.requiredField "battle" "Resolved battle state." Codec..= (.battle)
                <*> Codec.optionalFieldOrNull "inventory" "Updated inventory, only when changed." Codec..= (.inventory)
                <*> Codec.optionalFieldOrNull "party" "Updated player party, only when changed." Codec..= (.party)
                <*> Codec.optionalFieldOrNull "seed" "Advanced random seed, only when changed." Codec..= (.seed)
