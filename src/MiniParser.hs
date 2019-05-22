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
        <|> try (TChar <$rword "Char")

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
exprParser = makeExprParser termParser operator
        <|> try ifExprParser
        <|> try letExprParser
        <|> try letrecExprParser
        <|> try lambdaExprParser
        -- <|> try caseExprParser
        <|> try applyExprParser
        <|> try variableExprParser
        <|> try (parensParser exprParser)

termParser :: Parser Expr
termParser = try (EBoolLit True <$ rword "True")
        <|> try (EBoolLit False <$ rword "False")
        <|> try (EIntLit <$> integerParser)
        <|> try (ECharLit <$> charParser)
        <|> try (EVar <$> identifierParser)
        <|> try (parensParser exprParser)

operator :: [[Operator Parser Expr]]     
operator = 
  [
    [ Prefix (ENot <$ rword "not") ],

    [ InfixL (EAnd <$ rword "and"),
      InfixL (EOr <$ rword "or") ],

    [ InfixL (EMul <$ symbol "*"),
      InfixL (EDiv <$ symbol "/"),
      InfixL (EMod <$ symbol "%") ],

    [ InfixL (EAdd <$ symbol "+"),
      InfixL (ESub <$ symbol "-") ],

    [ InfixL (EGt <$ symbol ">"),
      InfixL (ELt <$ symbol "<"),
      InfixL (EGe <$ symbol ">="),
      InfixL (ELe <$ symbol "<=") ],

    [ InfixL (EEq <$ symbol "=="), 
      InfixL (ENeq <$ symbol "/=") ]
  ]

ifExprParser :: Parser Expr
ifExprParser = try $ do
    rword "if"
    e1 <- exprParser
    rword "then"
    e2 <- exprParser
    rword "else"
    e3 <- exprParser
    return (EIf e1 e2 e3)

letExprParser :: Parser Expr
letExprParser = try $ do
    rword "let"
    varname <- identifierParser
    void (symbol ":=")
    e2 <- exprParser
    rword "in"
    expr <- exprParser
    return (ELet (varname, e2) expr)

letrecExprParser :: Parser Expr
letrecExprParser = try $ do
    rword "letrec"
    returntype <- typeParser
    rword "def"
    funcname <- identifierParser
    void (symbol "(")
    argname <- identifierParser
    void (symbol "::")
    argtype <- typeParser
    void (symbol ")")
    void (symbol "{")
    funcbody <- exprParser
    void (symbol "}")
    rword "in"
    expr <- exprParser
    return (ELetRec funcname (argname, argtype) (funcbody, returntype) expr)

lambdaExprParser :: Parser Expr
lambdaExprParser = try $ do
    void (symbol "\\")
    varname <- identifierParser
    void (symbol "::")
    vartype <- typeParser
    void (symbol "->")
    expr <- exprParser
    return (ELambda (varname, vartype) expr)

variableExprParser :: Parser Expr
variableExprParser = try $ do
    void (symbol "$")
    varname <- identifierParser
    return (EVar varname)

applyExprParser :: Parser Expr
applyExprParser = try $ do
    void (symbol "|")
    e1 <- exprParser
    rword  "$"
    e2 <- exprParser
    return (EApply e1 e2)


main :: IO ()
main = 
  -- input <- getContents
  case runParser exprParser "" "letrec Int def inc(x::Int){x+1} in | inc $ 3" of
    Left error -> print error
    Right a -> print a
  -- parseTest lambdaExprParser "\\x::Int -> $x"
  -- parseTest letrecExprParser "letrec int def inc(x::int){x+1} in inc 3"



