{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

import Control.Applicative
import Data.Ratio ((%))
import Text.RawString.QQ
import Text.Trifecta
import Data.Ratio ((%))

type NumberOrString = Either Integer String

eitherOr :: String
eitherOr = [r|
123
abc
456
def|]

parseNos :: Parser NumberOrString
parseNos =
  skipMany (oneOf "\n") >>
  (Left <$> integer) <|> (Right <$> some letter)

main = do
  let p f i = parseString f mempty i
  print $ p parseNos eitherOr

virtuousFraction :: Parser Rational
virtuousFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be zero"
    _ -> return (numerator % denominator)

decimalOrRatio :: Parser (Either Integer Rational)
decimalOrRatio = try (Right <$> virtuousFraction) <|> (Left <$> decimal)
