module Critters.State.Message (Argument (..), BattleMessage (..), Message (..), MessageKey (..), Messages (..), Presentation (..), Template (..)) where

import Autodocodec (Autodocodec (..), HasCodec)
import Autodocodec qualified as Codec
import Critters.Expression (Value)
import Data.Aeson qualified as Aeson
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import Data.String (IsString)
import Data.Text (Text)

-- | Only explicit references are expanded; literal arguments remain literal.
data Argument = Literal Value | Reference Template
    deriving stock (Eq, Show)
    deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec Argument)

data BattleMessage = BattleMessage
    { message :: Message
    , presentation :: Presentation
    }
    deriving stock (Eq, Show)
    deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec BattleMessage)

data Message = Message
    { arguments :: Map Text Argument
    , key :: MessageKey
    }
    deriving stock (Eq, Show)
    deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec Message)

newtype MessageKey = MessageKey Text
    deriving stock (Eq, Ord, Show)
    deriving newtype (Aeson.FromJSONKey, Aeson.ToJSONKey, IsString)
    deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec MessageKey)

newtype Messages = Messages {templates :: Map MessageKey Template}
    deriving stock (Eq, Show)
    deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec Messages)

data Presentation = Dialogue | HealthChange
    deriving stock (Eq, Show)
    deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec Presentation)

newtype Template = Template Text
    deriving stock (Eq, Show)
    deriving newtype (IsString)
    deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec Template)

instance HasCodec Argument where
    codec = Codec.dimapCodec (either Literal Reference) encode (Codec.eitherCodec Codec.codec reference)
      where
        encode (Literal value) = Left value
        encode (Reference value) = Right value
        reference = Codec.object "MessageReference" (Codec.requiredField "reference" "Explicit template reference." Codec..= id)

instance HasCodec BattleMessage where
    codec = Codec.object "BattleMessage" $ BattleMessage <$> Codec.requiredField "message" "Message identity and arguments." Codec..= (.message) <*> Codec.requiredField "presentation" "Presentation independent of wording." Codec..= (.presentation)

instance HasCodec Message where
    codec = Codec.object "Message" $ Message <$> Codec.optionalFieldWithOmittedDefault "arguments" mempty "Local message arguments." Codec..= (.arguments) <*> Codec.requiredField "key" "Game-defined message key." Codec..= (.key)

instance HasCodec MessageKey where
    codec = Codec.dimapCodec MessageKey (\(MessageKey value) -> value) Codec.codec

instance HasCodec Messages where
    codec = Codec.dimapCodec Messages (.templates) Codec.codec

instance HasCodec Presentation where
    codec = Codec.stringConstCodec ((Dialogue, "dialogue") :| [(HealthChange, "health-change")])

instance HasCodec Template where
    codec = Codec.dimapCodec Template (\(Template value) -> value) Codec.codec
