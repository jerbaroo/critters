module Critters.State.Item (defaultItem, Item (..)) where

import Autodocodec (Autodocodec (..), HasCodec)
import Autodocodec qualified as Codec
import Critters.Codec qualified as NumberCodec
import Data.Aeson qualified as Aeson
import Data.Text (Text)

defaultItem :: Item
defaultItem =
    Item
        { captureBonus = 0
        , cureStatus = False
        , healing = 0
        , healthFraction = 0
        , name = ""
        , restoreUses = 0
        , reviveFraction = 0
        }

data Item = Item
    { captureBonus :: Double
    , cureStatus :: Bool
    , healing :: Int
    , healthFraction :: Double
    , name :: Text
    , restoreUses :: Int
    , reviveFraction :: Double
    }
    deriving stock (Eq)
    deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec Item)

instance HasCodec Item where
    codec =
        Codec.bimapCodec validate id $
            Codec.object "Item" $
                Item
                    <$> Codec.optionalFieldWithOmittedDefaultWith "captureBonus" NumberCodec.finiteDoubleCodec (0) "Positive values mark a capture item and scale capture probability." Codec..= (.captureBonus)
                    <*> Codec.optionalFieldWithOmittedDefault "cureStatus" (False) "Clear a persistent status." Codec..= (.cureStatus)
                    <*> Codec.optionalFieldWithOmittedDefault "healing" (0) "Flat HP restored to a conscious target." Codec..= (.healing)
                    <*> Codec.optionalFieldWithOmittedDefaultWith "healthFraction" NumberCodec.probabilityCodec (0) "Additional maximum HP fraction restored." Codec..= (.healthFraction)
                    <*> Codec.optionalFieldWithOmittedDefault "name" ("") "Display name." Codec..= (.name)
                    <*> Codec.optionalFieldWithOmittedDefault "restoreUses" (0) "Uses restored to each known move." Codec..= (.restoreUses)
                    <*> Codec.optionalFieldWithOmittedDefaultWith "reviveFraction" NumberCodec.probabilityCodec (0) "HP fraction restored to a fainted target." Codec..= (.reviveFraction)
      where
        validate value
            | value.captureBonus < 0 || value.healing < 0 || value.restoreUses < 0 = Left "Item strengths must not be negative."
            | otherwise = Right value
