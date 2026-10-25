{- | Pure expressions and text interpolation with caller-supplied placeholders.
Numeric expressions support arithmetic, comparisons (zero or one), and the
functions abs, ceil, clamp, floor, if, max, min, round, and sqrt. The if
function evaluates only the selected branch, which may yield text or a number.
Placeholders use identifiers such as stage or targetHP; text values are
supplied by the caller rather than written as string literals in expressions.
-}
module Critters.Expression (evaluate, evaluateNumber, Expression (..), interpolate, Value (..), variables) where

import Autodocodec (Autodocodec (..), HasCodec)
import Autodocodec qualified as Codec
import Data.Aeson qualified as Aeson
import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.List (nub)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Text.ParserCombinators.ReadP qualified as P
import Witch (from, tryFrom)

evaluate :: Map Text Value -> Expression -> Either Text Value
evaluate environment expression = parse expression >>= compute
  where
    compute = \case
        Binary operator left right -> do
            a <- compute left >>= numeric
            b <- compute right >>= numeric
            checked =<< case operator of
                "+" -> Right (a + b)
                "-" -> Right (a - b)
                "*" -> Right (a * b)
                "/" | b /= 0 -> Right (a / b)
                "/" -> Left "Division by zero in an expression."
                "^" -> Right (a ** b)
                "<" -> Right (truth (a < b))
                "<=" -> Right (truth (a <= b))
                ">" -> Right (truth (a > b))
                ">=" -> Right (truth (a >= b))
                "==" -> Right (truth (a == b))
                "!=" -> Right (truth (a /= b))
                _ -> Left "Unknown expression operator."
        Call "if" [condition, yes, no] -> do
            value <- compute condition >>= numeric
            compute (if value /= 0 then yes else no)
        Call function arguments -> do
            values <- traverse (\argument -> compute argument >>= numeric) arguments
            checked =<< case (function, values) of
                ("abs", [value]) -> Right (abs value)
                ("ceil", [value]) -> integer (ceiling value)
                ("clamp", [low, high, value]) -> Right (max low (min high value))
                ("floor", [value]) -> integer (floor value)
                ("max", [a, b]) -> Right (max a b)
                ("min", [a, b]) -> Right (min a b)
                ("round", [value]) -> integer (round value)
                ("sqrt", [value]) -> Right (sqrt value)
                _ -> Left "Invalid expression function."
        Number value -> checked value
        Variable name -> case Map.lookup name environment of
            Just (NumberValue value) -> checked value
            Just value -> Right value
            Nothing -> Left ("Unknown expression placeholder: " <> name)
    checked value
        | isNaN value || isInfinite value = Left "An expression produced a non-finite number."
        | otherwise = Right (NumberValue value)
    integer value = either (const (Left "Rounded expression value is out of range.")) Right (tryFrom @Integer @Double value)
    truth value = if value then 1 else 0

-- | Evaluate a numeric rule, rejecting text results and invalid placeholders.
evaluateNumber :: Map Text Double -> Expression -> Either Text Double
evaluateNumber environment expression = evaluate (Map.map NumberValue environment) expression >>= numeric

newtype Expression = Expression Text
    deriving stock (Eq, Show)
    deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec Expression)

{- | Evaluate braced expressions such as @{subject}: {before - after}@.
Whole numbers render without a decimal suffix. Inserted text is literal;
malformed braces, unknown placeholders, and invalid expressions return errors.
-}
interpolate :: Map Text Value -> Text -> Either Text Text
interpolate environment = go
  where
    go remaining =
        let (prefix, suffix) = Text.break (\character -> character == '{' || character == '}') remaining
         in case Text.uncons suffix of
                Nothing -> Right prefix
                Just ('}', _) -> Left "Unexpected closing brace in expression template."
                Just (_, afterOpen) -> do
                    let (source, afterExpression) = Text.breakOn "}" afterOpen
                    case Text.uncons afterExpression of
                        Nothing -> Left "Unclosed expression placeholder in template."
                        Just (_, rest) -> do
                            value <- evaluate environment (Expression source)
                            rendered <- go rest
                            Right (prefix <> render value <> rendered)
    render (NumberValue value) =
        let (whole, fraction) = properFraction value :: (Integer, Double)
         in from (if fraction == 0 then show whole else show value)
    render (TextValue value) = value

numeric :: Value -> Either Text Double
numeric = \case
    NumberValue value -> Right value
    TextValue _ -> Left "Expected a number in expression, but received text."

parse :: Expression -> Either Text Term
parse (Expression source) = case P.readP_to_S (spaces *> comparison <* P.eof) (from source) of
    [] -> Left ("Invalid expression: " <> source)
    results -> validate (fst (last results))
  where
    additive = P.chainl1 multiplicative (operator ["+", "-"])
    arguments = P.between (symbol "(") (symbol ")") (P.sepBy comparison (symbol ","))
    atom =
        P.between (symbol "(") (symbol ")") comparison
            P.<++ ( do
                        remaining <- P.look
                        case remaining of
                            first : _ | isDigit first -> Number <$> lexeme (P.readS_to_P (reads :: ReadS Double))
                            _ -> P.pfail
                  )
            P.<++ do
                first <- P.satisfy (\value -> isAlpha value || value == '_')
                rest <- P.munch (\value -> isAlphaNum value || value == '_')
                spaces
                let name = from (first : rest)
                (Call name <$> arguments) P.<++ pure (Variable name)
    comparison = P.chainl1 additive (operator ["<=", ">=", "==", "!=", "<", ">"])
    lexeme parser = parser <* spaces
    multiplicative = P.chainl1 unary (operator ["*", "/"])
    operator values = P.choice [Binary value <$ symbol (from value) | value <- values]
    power = do
        base <- atom
        (Binary "^" base <$> (symbol "^" *> unary)) P.<++ pure base
    spaces = P.skipSpaces
    symbol = lexeme . P.string
    unary = (symbol "-" *> (Binary "-" (Number 0) <$> unary)) P.<++ (symbol "+" *> unary) P.<++ power
    validate expression = case expression of
        Binary _ left right -> validate left >> validate right >> Right expression
        Call name values
            | Map.lookup name arities == Just (length values) -> traverse validate values >> Right expression
            | otherwise -> Left ("Unknown function or incorrect argument count: " <> name)
        Number value | isNaN value || isInfinite value -> Left "Expression constants must be finite."
        _ -> Right expression
    arities = Map.fromList [("abs", 1), ("ceil", 1), ("clamp", 3), ("floor", 1), ("if", 3), ("max", 2), ("min", 2), ("round", 1), ("sqrt", 1)]

data Term
    = Binary Text Term Term
    | Call Text [Term]
    | Number Double
    | Variable Text

data Value
    = NumberValue Double
    | TextValue Text
    deriving stock (Eq, Show)

variables :: Expression -> Either Text [Text]
variables formula = nub . collect <$> parse formula
  where
    collect = \case
        Binary _ left right -> collect left <> collect right
        Call _ arguments -> concatMap collect arguments
        Number _ -> []
        Variable name -> [name]

instance HasCodec Expression where
    codec = Codec.bimapCodec validate (\(Expression source) -> source) Codec.codec
      where
        validate source
            | Text.length source > 2048 = Left "Expressions must not exceed 2048 characters."
            | otherwise = let formula = Expression source in either (Left . from) (const (Right formula)) (parse formula)
