module Critters.State.ActiveInteraction (ActiveInteraction (..)) where

import Autodocodec (Autodocodec (..), HasCodec)
import Autodocodec qualified as Codec
import Critters.State.Interaction (Interaction)
import Critters.State.Message (Messages)
import Data.Aeson qualified as Aeson

-- | A snapshot of the selected interaction and its current presentation.
data ActiveInteraction = ActiveInteraction
    { dialogueIndex :: Int
    , messageTemplates :: Messages
    , script :: Interaction
    , selectedChoice :: Maybe Int
    }
    deriving stock (Eq)
    deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec ActiveInteraction)

instance HasCodec ActiveInteraction where
    codec =
        Codec.object "ActiveInteraction" $
            ActiveInteraction
                <$> Codec.optionalFieldWithOmittedDefault "dialogueIndex" 0 "The current dialogue line index." Codec..= (.dialogueIndex)
                <*> Codec.requiredField "messageTemplates" "Message catalogue captured with the selected script." Codec..= (.messageTemplates)
                <*> Codec.requiredField "script" "The interaction selected when this conversation started." Codec..= (.script)
                <*> Codec.optionalFieldOrNull "selectedChoice" "The selected choice index; absent while reading dialogue." Codec..= (.selectedChoice)
