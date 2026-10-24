module Critters.State.ActiveInteraction (ActiveInteraction (..)) where

import Autodocodec (Autodocodec (..), HasCodec)
import Autodocodec qualified as Codec
import Critters.State.Interaction (Interaction)
import Data.Aeson qualified as Aeson

-- | A snapshot of the selected interaction and its current presentation.
data ActiveInteraction = ActiveInteraction
  { activeDialogueIndex :: Int,
    activeScript :: Interaction,
    activeSelectedChoice :: Maybe Int
  }
  deriving stock (Eq)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec ActiveInteraction)

instance HasCodec ActiveInteraction where
  codec =
    Codec.object "ActiveInteraction" $
      ActiveInteraction
        <$> Codec.optionalFieldWithOmittedDefault "dialogueIndex" 0 "The current dialogue line index." Codec..= activeDialogueIndex
        <*> Codec.requiredField "script" "The interaction selected when this conversation started." Codec..= activeScript
        <*> Codec.optionalFieldOrNull "selectedChoice" "The selected choice index; absent while reading dialogue." Codec..= activeSelectedChoice
