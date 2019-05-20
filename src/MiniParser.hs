module MiniParser where

import AST
import Util

import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

-- common used components
rword :: String -> Parser ()
rword w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

reserve :: [String] -- list of reserve words
reserve = ["True","False","not","and","or","add","sub","mul","div","mod",
            "if","then","else","let","in","letrec","where","case","Data"]

sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parensParser :: Parser a -> Parser a
parensParser = between (symbol "(") (symbol ")")

integerParser :: Parser Int
integerParser = lexeme L.decimal

charParser :: Parser Char
charParser = between (symbol "\'") (symbol "\'") (lexeme L.charLiteral)

identifierParser :: Parser String
identifierParser =  (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` reserve
              then fail $ "keyword " ++ show x ++ " cannot be an identifier!"
              else return x

-- root parser of all expressions
programParser :: Parser Expr
programParser = between sc eof exprParser

exprParser :: Parser Expr
exprParser = boolExprParser
        <|> integerExprParser
        <|> charExprParser
        -- <|> ifExprParser
        -- <|> letExprParser
        -- <|> letRecExprParser
        -- <|> lambdaExprParser
        -- <|> caseExprParser
        -- <|> applyExprParser
        <|> parensParser exprParser

boolExprParser :: Parser Expr
boolExprParser = mytrace "bool expr" makeExprParser boolTermParser boolOperator

integerExprParser :: Parser Expr
integerExprParser = mytrace "integer expr" makeExprParser integerTermParser integerOperator

charExprParser :: Parser Expr
charExprParser = parensParser charExprParser
        <|> charTermParser

-- parsers of terms
boolTermParser :: Parser Expr
boolTermParser = parensParser boolExprParser
        -- <|> EVar <$> identifierParser
        <|> (EBoolLit True <$ rword "True")
        <|> (EBoolLit False <$ rword "False")
        <|> comparisonExprParser

integerTermParser :: Parser Expr
integerTermParser = parensParser integerExprParser
        -- <|> EVar <$> identifierParser
        <|> EIntLit <$> integerParser

charTermParser :: Parser Expr
charTermParser = parensParser charExprParser
        -- <|> EVar <$> identifierParser
        <|> ECharLit <$> charParser

-- parsers of operators
boolOperator :: [[Operator Parser Expr]]
boolOperator =
  [ [ Prefix (ENot <$ rword "not") ],

    [ InfixL (EAnd <$ rword "and"),
      InfixL (EOr <$ rword "or") ]
  ]

integerOperator :: [[Operator Parser Expr]]
integerOperator =
  [
    [ InfixL (EMul <$ symbol "*"),
      InfixL (EDiv <$ symbol "/"),
      InfixL (EMod <$ symbol "%") ],

    [ InfixL (EAdd <$ symbol "+")
    , InfixL (ESub <$ symbol "-") ]
  ]

-- parser of comparision
comparisonExprParser :: Parser Expr
comparisonExprParser = boolEqParser
    <|> boolNeqParser
    <|> integerEqParser
    <|> integerNeqParser
    <|> integerGtParser
    <|> integerLtParser
    <|> integerGeParser
    <|> integerLeParser
    <|> charEqParser
    <|> charNeqParser
    <|> charGtParser
    <|> charLtParser
    <|> charGeParser
    <|> charLeParser

boolEqParser :: Parser Expr
boolEqParser = do
    e1 <- boolExprParser
    symbol "="
    e2 <- boolExprParser
    return (EEq e1 e2)

boolNeqParser :: Parser Expr
boolNeqParser = do
    e1 <- boolExprParser
    symbol "/="
    e2 <- boolExprParser
    return (ENeq e1 e2)

integerEqParser :: Parser Expr
integerEqParser = do
    e1 <- integerExprParser
    symbol "=="
    e2 <- integerExprParser
    return (EEq e1 e2)

integerNeqParser :: Parser Expr
integerNeqParser = do
    e1 <- integerExprParser
    symbol "/="
    e2 <- integerExprParser
    return (ENeq e1 e2)

integerGtParser :: Parser Expr
integerGtParser = do
    e1 <- integerExprParser
    symbol ">"
    e2 <- integerExprParser
    return (EGt e1 e2)

integerLtParser :: Parser Expr
integerLtParser = do
    e1 <- integerExprParser
    symbol "<"
    e2 <- integerExprParser
    return (ELt e1 e2)

integerGeParser :: Parser Expr
integerGeParser = do
    e1 <- integerExprParser
    symbol ">="
    e2 <- integerExprParser
    return (EGe e1 e2)

integerLeParser :: Parser Expr
integerLeParser = do
    e1 <- integerExprParser
    symbol "<="
    e2 <- integerExprParser
    return (ELe e1 e2)

charEqParser :: Parser Expr
charEqParser = do
    e1 <- charExprParser
    symbol "=="
    e2 <- charExprParser
    return (EEq e1 e2)

charNeqParser :: Parser Expr
charNeqParser = do
    e1 <- charExprParser
    symbol "/="
    e2 <- charExprParser
    return (ENeq e1 e2)

charGtParser :: Parser Expr
charGtParser = do
    e1 <- charExprParser
    symbol ">"
    e2 <- charExprParser
    return (EGt e1 e2)

charLtParser :: Parser Expr
charLtParser = do
    e1 <- charExprParser
    symbol "<"
    e2 <- charExprParser
    return (ELt e1 e2)

charGeParser :: Parser Expr
charGeParser = do
    e1 <- charExprParser
    symbol ">="
    e2 <- charExprParser
    return (EGe e1 e2)

charLeParser :: Parser Expr
charLeParser = do
    e1 <- charExprParser
    symbol "<="
    e2 <- charExprParser
    return (ELe e1 e2)


main :: IO ()
main = do
  input <- getContents
  parseTest programParser input



