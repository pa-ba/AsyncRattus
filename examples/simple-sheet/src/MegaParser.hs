module MegaParser (
    parse,
) where

import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L hiding (binary) 
import Control.Monad.Combinators.Expr
import Control.Applicative ((*>))
import Control.Monad (void)
import Control.Applicative hiding (many, some)
import Data.Void

import Expr

data ParseError = 
    UnrecognizedOperator String |
    UnrecognizedFormat String


type Parser = Parsec Void String

parse :: String -> Maybe Expr
parse = parseMaybe pExpr

-- based on example https://markkarpov.com/tutorial/megaparsec.html#parsing-expressions

lexeme = L.lexeme whitespace
symbol = L.symbol whitespace

whitespace :: Parser ()
whitespace = void $ many $ (oneOf " \t" <?> "whitespace")

-- Parse variable of form a1 etc
pVar :: Parser Expr
pVar = Var <$> lexeme ((:) <$> letterChar <*> some digitChar <?> "Variable")

pInt :: Parser Expr
pInt = Number <$> lexeme L.decimal

pTerm :: Parser Expr
pTerm = choice [pVar, pInt, pExpr]

pExpr :: Parser Expr
pExpr = makeExprParser pTerm operatorTable

operatorTable :: [[Operator Parser Expr]]
operatorTable = [[prefix "-" Negate, prefix "+" id],
                 [binary "*" Times, binary "/" Divide],
                 [binary "+" Plus, binary "-" Minus]]

prefix :: String -> (Expr -> Expr) -> Operator Parser Expr
prefix name f = Prefix (f <$ symbol name)

binary :: String -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary name f = InfixL (f <$ symbol name)