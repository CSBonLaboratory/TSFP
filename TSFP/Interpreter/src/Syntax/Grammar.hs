module Syntax.Grammar where

import Syntax.Parser
import Syntax.Expression
import Control.Applicative (many, some, (<|>))

import Data.Char

parseProgram :: String -> Maybe [Expression]
parseProgram prog = parse ((many lineParser)) prog

-- a line can start have either an expression or can be some whitespace characters
lineParser :: Parser Expression
lineParser = (many (spot isSpace)) *> outerExprParser <* (many (spot isSpace))

-- parser that recognizes all types of expressions that can be on a line
outerExprParser :: Parser Expression
outerExprParser = bindingParser <|> innerExprParser

-- parser only for expressions that can be met inside other expressions
innerExprParser :: Parser Expression
innerExprParser = applicationParser <|> symbolParser <|> lambdaParser

{-- Binding :: String -> Expression -> Expression
    <$> :: (a -> b) -> Parser a -> Parser b
    some letter :: Parser String 
    => Binding <$> (some letter ) :: Parser (Expression -> Expression)
    Parser (Expression -> Expression) <*> Parser Expression => Parser Expression

    Parse some letters which represent the symbol, ignore = and then parse inner expression
--}
bindingParser :: Parser Expression
bindingParser = Binding <$> (some letter) <*> (token '=' *> (applicationParser  <|> symbolParser <|> lambdaParser))

applicationParser :: Parser Expression
applicationParser = Application <$> (token '(' *> innerExprParser ) <*> (many (spot isSpace) *> innerExprParser <* token ')')

symbolParser :: Parser Expression
symbolParser = Symbol <$> (some letter)

lambdaParser :: Parser Expression
lambdaParser = Lambda <$> (token '\\' *> (some letter)) <*> (token '.' *> innerExprParser)

