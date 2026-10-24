module Critters.State.Interaction (Interaction (..)) where

import Autodocodec (Autodocodec (..), HasCodec)
import Autodocodec qualified as Codec
import Critters.State.Choice (Choice)
import Critters.State.Condition (Condition)
import Critters.State.Effect (Effect)
import Data.Aeson qualified as Aeson
import Data.Text (Text)

data Interaction = Interaction
  { interactionChoices :: [Choice],
    interactionConditions :: [Condition],
    interactionDialogue :: [Text],
    interactionEffects :: [Effect]
  }
  deriving stock (Eq)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec Interaction)

instance HasCodec Interaction where
  codec =
    Codec.named "Interaction"
      $ Codec.object "Interaction"
      $ Interaction
        <$> Codec.optionalFieldWithOmittedDefault "choices" [] "Choices offered after the dialogue." Codec..= interactionChoices
        <*> Codec.optionalFieldWithOmittedDefault "conditions" [] "All conditions must hold; the first matching interaction is used." Codec..= interactionConditions
        <*> Codec.optionalFieldWithOmittedDefault "dialogue" [] "Dialogue lines in order." Codec..= interactionDialogue
        <*> Codec.optionalFieldWithOmittedDefault "effects" [] "Effects applied on completion, together with the confirmed choice's effects." Codec..= interactionEffects
