-- | Small, pure arithmetic expressions for game-defined rules.
module Critters.Formula (evaluate, Formula (..), variables) where

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

evaluate :: Map Text Double -> Formula -> Either Text Double
evaluate environment formula = parse formula >>= compute
  where
    compute = \case
      Binary operator left right -> do
        a <- compute left
        b <- compute right
        checked =<< case operator of
          "+" -> Right (a + b)
          "-" -> Right (a - b)
          "*" -> Right (a * b)
          "/" | b /= 0 -> Right (a / b)
          "/" -> Left "Division by zero in a rule."
          "^" -> Right (a ** b)
          "<" -> Right (truth (a < b))
          "<=" -> Right (truth (a <= b))
          ">" -> Right (truth (a > b))
          ">=" -> Right (truth (a >= b))
          "==" -> Right (truth (a == b))
          "!=" -> Right (truth (a /= b))
          _ -> Left "Unknown rule operator."
      Call "if" [condition, yes, no] -> compute condition >>= \value -> compute (if value /= 0 then yes else no)
      Call function arguments -> do
        values <- traverse compute arguments
        checked =<< case (function, values) of
          ("abs", [value]) -> Right (abs value)
          ("ceil", [value]) -> integer (ceiling value)
          ("clamp", [low, high, value]) -> Right (max low (min high value))
          ("floor", [value]) -> integer (floor value)
          ("max", [a, b]) -> Right (max a b)
          ("min", [a, b]) -> Right (min a b)
          ("round", [value]) -> integer (round value)
          ("sqrt", [value]) -> Right (sqrt value)
          _ -> Left "Invalid rule function."
      Number value -> checked value
      Variable name -> maybe (Left ("Unknown rule variable: " <> name)) checked (Map.lookup name environment)
    checked value
      | isNaN value || isInfinite value = Left "A rule produced a non-finite number."
      | otherwise = Right value
    integer value = either (const (Left "Rounded rule value is out of range.")) Right (tryFrom @Integer @Double value)
    truth value = if value then 1 else 0

data Expression
  = Binary Text Expression Expression
  | Call Text [Expression]
  | Number Double
  | Variable Text

newtype Formula = Formula Text
  deriving stock (Eq)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec Formula)

parse :: Formula -> Either Text Expression
parse (Formula source) = case P.readP_to_S (spaces *> comparison <* P.eof) (from source) of
  [] -> Left ("Invalid rule expression: " <> source)
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
      Number value | isNaN value || isInfinite value -> Left "Rule constants must be finite."
      _ -> Right expression
    arities = Map.fromList [("abs", 1), ("ceil", 1), ("clamp", 3), ("floor", 1), ("if", 3), ("max", 2), ("min", 2), ("round", 1), ("sqrt", 1)]

variables :: Formula -> Either Text [Text]
variables formula = nub . collect <$> parse formula
  where
    collect = \case
      Binary _ left right -> collect left <> collect right
      Call _ arguments -> concatMap collect arguments
      Number _ -> []
      Variable name -> [name]

instance HasCodec Formula where
  codec = Codec.bimapCodec validate (\(Formula source) -> source) Codec.codec
    where
      validate source
        | Text.length source > 2048 = Left "Rule expressions must not exceed 2048 characters."
        | otherwise = let formula = Formula source in either (Left . from) (const (Right formula)) (parse formula)
