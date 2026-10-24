module Critters.State.BattleUpdate (BattleUpdate (..)) where

import Autodocodec qualified as Codec
import Autodocodec (Autodocodec (..), HasCodec)
import Critters.State.Battle (Battle)
import Critters.State.Party (Party)
import Data.Aeson qualified as Aeson
import Data.Map.Strict (Map)
import Data.Text (Text)

data BattleUpdate = BattleUpdate
  { updateBattle :: Battle,
    updateInventory :: Maybe (Map Text Int),
    updateParty :: Maybe Party,
    updateSeed :: Maybe Int
  }
  deriving stock (Eq)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec BattleUpdate)

instance HasCodec BattleUpdate where
  codec = Codec.object "BattleUpdate" $ BattleUpdate
    <$> Codec.requiredField "battle" "Resolved battle state." Codec..= updateBattle
    <*> Codec.optionalFieldOrNull "inventory" "Updated inventory, only when changed." Codec..= updateInventory
    <*> Codec.optionalFieldOrNull "party" "Updated player party, only when changed." Codec..= updateParty
    <*> Codec.optionalFieldOrNull "seed" "Advanced random seed, only when changed." Codec..= updateSeed
