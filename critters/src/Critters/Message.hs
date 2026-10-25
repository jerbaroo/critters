module Critters.Message (named, numeric, reference, renderMessage, renderText, text) where

import Critters.Expression (Value (..))
import Critters.Expression qualified as Expression
import Critters.Number (number)
import Critters.State.Message (Argument (..), Message (..), MessageKey (..), Messages (..), Template (..))
import Data.Map.Lazy qualified as LazyMap
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text

named :: MessageKey -> [(Text, Argument)] -> Message
named key arguments = Message (Map.fromList arguments) key

numeric :: Int -> Argument
numeric = Literal . NumberValue . number

reference :: Text -> Argument
reference = Reference . Template

renderMessage :: Messages -> Map Text Value -> Message -> Either Text Text
renderMessage messages environment message =
    let MessageKey key = message.key
     in renderWith messages environment message.arguments ("{messages." <> key <> "}")

-- | Follow only explicit catalogue/content references; detect cycles by path.
renderText :: Messages -> Map Text Value -> Text -> Either Text Text
renderText messages environment = renderWith messages environment Map.empty

renderWith :: Messages -> Map Text Value -> Map Text Argument -> Text -> Either Text Text
renderWith messages environment arguments = expand Set.empty
  where
    expand seen = Expression.interpolateWith (resolve seen)
    follow seen name template
        | Set.member name seen = Left ("Cyclic message reference: " <> name)
        | otherwise = TextValue <$> expand (Set.insert name seen) template
    resolve seen name
        | Just argument <- Map.lookup name arguments = case argument of
            Literal value -> Right value
            Reference (Template template) -> follow seen ("argument." <> name) template
        | Just key <- Text.stripPrefix "messages." name = case Map.lookup (MessageKey key) messages.templates of
            Nothing -> Left ("Missing message: " <> key)
            Just (Template template) -> follow seen name template
        | otherwise = case LazyMap.lookup name environment of
            Nothing -> Left ("Unknown expression placeholder: " <> name)
            Just (TextValue template) | any (`Text.isPrefixOf` name) ["npcs.", "combat."] -> follow seen name template
            Just value -> Right value

text :: Text -> Argument
text = Literal . TextValue
