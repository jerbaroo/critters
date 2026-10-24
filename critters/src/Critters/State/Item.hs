module Critters.State.Item (defaultItem, Item (..)) where

import Autodocodec (Autodocodec (..), HasCodec)
import Autodocodec qualified as Codec
import Critters.Codec qualified as NumberCodec
import Data.Aeson qualified as Aeson
import Data.Text (Text)

defaultItem :: Item
defaultItem =
  Item
    { itemCaptureBonus = 0,
      itemCureStatus = False,
      itemHealing = 0,
      itemHealthFraction = 0,
      itemName = "",
      itemRestoreUses = 0,
      itemReviveFraction = 0
    }

data Item = Item
  { itemCaptureBonus :: Double,
    itemCureStatus :: Bool,
    itemHealing :: Int,
    itemHealthFraction :: Double,
    itemName :: Text,
    itemRestoreUses :: Int,
    itemReviveFraction :: Double
  }
  deriving stock (Eq)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec Item)

instance HasCodec Item where
  codec =
    Codec.bimapCodec validate id
      $ Codec.object "Item"
      $ Item
        <$> Codec.optionalFieldWithOmittedDefaultWith "captureBonus" NumberCodec.finiteDoubleCodec (0) "Positive values mark a capture item and scale capture probability." Codec..= itemCaptureBonus
        <*> Codec.optionalFieldWithOmittedDefault "cureStatus" (False) "Clear a persistent status." Codec..= itemCureStatus
        <*> Codec.optionalFieldWithOmittedDefault "healing" (0) "Flat HP restored to a conscious target." Codec..= itemHealing
        <*> Codec.optionalFieldWithOmittedDefaultWith "healthFraction" NumberCodec.probabilityCodec (0) "Additional maximum HP fraction restored." Codec..= itemHealthFraction
        <*> Codec.optionalFieldWithOmittedDefault "name" ("") "Display name." Codec..= itemName
        <*> Codec.optionalFieldWithOmittedDefault "restoreUses" (0) "Uses restored to each known move." Codec..= itemRestoreUses
        <*> Codec.optionalFieldWithOmittedDefaultWith "reviveFraction" NumberCodec.probabilityCodec (0) "HP fraction restored to a fainted target." Codec..= itemReviveFraction
    where
      validate value
        | itemCaptureBonus value < 0 || itemHealing value < 0 || itemRestoreUses value < 0 = Left "Item strengths must not be negative."
        | otherwise = Right value
