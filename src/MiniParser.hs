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

typeParser :: Parser Type
typeParser = try (TBool <$rword "Bool")
        <|> try (TInt <$rword "Int")
        <|> (TChar <$rword "Char")

integerParser :: Parser Int
integerParser = (lexeme . try) L.decimal

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
exprParser = try boolExprParser
        <|> try integerExprParser
        <|> try charExprParser
        <|> try ifExprParser
        <|> try letExprParser
        <|> try whereExprParser
        <|> try letrecExprParser
        <|> try lambdaExprParser
        -- <|> try caseExprParser
        <|> try applyExprParser
        <|> try variableExprParser
        <|> parensParser exprParser

boolExprParser :: Parser Expr
boolExprParser = makeExprParser boolTermParser boolOperator

integerExprParser :: Parser Expr
integerExprParser = makeExprParser integerTermParser integerOperator

charExprParser :: Parser Expr
charExprParser = charTermParser

-- parsers of terms
boolTermParser :: Parser Expr
boolTermParser =  try $ parensParser boolExprParser
        -- <|> EVar <$> identifierParser
        <|> try (EBoolLit True <$ rword "True")
        <|> try (EBoolLit False <$ rword "False")
        <|>  comparisonExprParser

integerTermParser :: Parser Expr
integerTermParser = try $ parensParser integerExprParser
        -- <|> EVar <$> identifierParser
        <|> EIntLit <$> integerParser

charTermParser :: Parser Expr
charTermParser = try $ parensParser charExprParser
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
comparisonExprParser = try boolEqParser
    <|> try boolNeqParser
    <|> try integerEqParser
    <|> try integerNeqParser
    <|> try integerGtParser
    <|> try integerLtParser
    <|> try integerGeParser
    <|> try integerLeParser
    <|> try charEqParser
    <|> try charNeqParser
    <|> try charGtParser
    <|> try charLtParser
    <|> try charGeParser
    <|>  charLeParser

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
    rword ">="
    e2 <- charExprParser
    return (EGe e1 e2)

charLeParser :: Parser Expr
charLeParser = do
    e1 <- charExprParser
    symbol "<="
    e2 <- charExprParser
    return (ELe e1 e2)


ifExprParser :: Parser Expr
ifExprParser = do
    rword "if"
    e1 <- boolExprParser
    rword "then"
    e2 <- exprParser
    rword "else"
    e3 <- exprParser
    return (EIf e1 e2 e3)

letExprParser :: Parser Expr
letExprParser = do
    rword "let"
    varname <- identifierParser
    symbol "="
    e2 <- exprParser
    rword "in"
    expr <- exprParser
    return (ELet (varname, e2) expr)

whereExprParser :: Parser Expr
whereExprParser = do
    expr <- exprParser
    rword "where"
    varname <- identifierParser
    symbol "="
    e2 <- exprParser
    return (ELet (varname, e2) expr)

letrecExprParser :: Parser Expr
letrecExprParser = do
    rword "letrec"
    returntype <- typeParser
    rword "def"
    funcname <- identifierParser
    symbol "("
    argname <- identifierParser
    symbol "::"
    argtype <- typeParser
    symbol ")"
    symbol "{"
    funcbody <- exprParser
    symbol "}"
    rword "in"
    expr <- exprParser
    return (ELetRec funcname (argname, argtype) (funcbody, returntype) expr)

lambdaExprParser :: Parser Expr
lambdaExprParser = do
    symbol "\\"
    varname <- identifierParser
    symbol "::"
    vartype <- typeParser
    symbol "->"
    expr <- exprParser
    return (ELambda (varname, vartype) expr)

variableExprParser :: Parser Expr
variableExprParser = do
    symbol "$"
    varname <- identifierParser
    return (EVar varname)

applyExprParser :: Parser Expr
applyExprParser = do
    e1 <- exprParser
    rword  "$"
    e2 <- exprParser
    return (EApply e1 e2)

main :: IO ()
main = 
  -- input <- getContents
  case runParser exprParser "" "1+2" of
    Left error -> print error
    Right a -> print a
  -- parseTest lambdaExprParser "\\x::Int -> $x"
  -- parseTest letrecExprParser "letrec int def inc(x::int){x+1} in inc 3"



