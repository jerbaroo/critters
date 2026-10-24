module Critters.State.Choice (Choice (..)) where

import Autodocodec (Autodocodec (..), HasCodec)
import Autodocodec qualified as Codec
import Critters.State.Effect (Effect)
import Data.Aeson qualified as Aeson
import Data.Text (Text)

data Choice = Choice
  { choiceEffects :: [Effect],
    choiceLabel :: Text
  }
  deriving stock (Eq)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec Choice)

instance HasCodec Choice where
  codec =
    Codec.object "Choice" $
      Choice
        <$> Codec.optionalFieldWithOmittedDefault "effects" [] "Effects applied when this choice is confirmed." Codec..= choiceEffects
        <*> Codec.requiredField "label" "The text displayed for this choice." Codec..= choiceLabel
