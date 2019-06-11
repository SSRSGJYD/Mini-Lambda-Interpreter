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
data BindExpr = BindExpr String Expr
data Arg = Arg String Type 

-- common used components
rword :: String -> Parser ()
rword w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

reserve :: [String] -- list of reserve words
reserve = ["True","False","not","and","or","add","sub","mul","div","mod",
            "if","then","else","let","in","letrec","where","case","data", "\\"]

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
typeParser = makeExprParser atomTypeParser arrowOperator
  
atomTypeParser :: Parser Type
atomTypeParser = try (TBool <$rword "Bool")
        <|> try (TInt <$rword "Int")
        <|> try (TChar <$rword "Char")
        <|> try (TData <$> identifierParser)
        <|> try (parensParser typeParser)

typesParser :: Parser [Type]
typesParser = sepBy typeParser (symbol ",")

arrowOperator :: [[Operator Parser Type]]
arrowOperator = [[ InfixL (TArrow <$ symbol "->")]]

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
exprParser :: Parser Expr
exprParser = makeExprParser termParser operator
        <|> try ifExprParser
        <|> try letExprParser
        <|> try letrecExprParser
        <|> try lambdaExprParser
        <|> try caseExprParser
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

-- let expression
bindsExprParser :: Parser [BindExpr]
bindsExprParser = sepBy1 bindExprParser (symbol ",")

bindExprParser :: Parser BindExpr
bindExprParser = try $ do
  varname <- identifierParser
  void (symbol "=")
  e <- exprParser
  return $ BindExpr varname e

letExprParser :: Parser Expr
letExprParser = try $ do
    rword "let"
    binds <- bindsExprParser
    rword "in"
    expr <- exprParser
    return $ goLet binds expr

goLet :: [BindExpr] -> Expr -> Expr
goLet [] e = e
goLet (bind:binds) e = ELet (varname, expr) $ goLet binds e
    where BindExpr varname expr = bind

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

-- lambda expression
argsParser :: Parser [Arg]
argsParser = sepBy1 argParser (symbol ",")

argParser :: Parser Arg
argParser = try $ do
  varname <- identifierParser
  void (symbol "::")
  vartype <- typeParser
  return $ Arg varname vartype

lambdaExprParser :: Parser Expr
lambdaExprParser = try $ do
    void (symbol "\\(")
    args <- argsParser
    void (symbol ")")
    void (symbol "->")
    expr <- exprParser
    return $ goLambda args expr

goLambda :: [Arg] -> Expr -> Expr
goLambda [] e = e
goLambda (arg:args) e = ELambda (varname, vartype) $ goLambda args e
    where Arg varname vartype = arg

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

-- deal with case expr and patterns
caseExprParser :: Parser Expr
caseExprParser = try $ do
    rword "case"
    e <- exprParser
    rword "of"
    patternAssigns <- patternAssignsParser
    return $ ECase e patternAssigns

-- patternAssign ; patternAssign ; ... ; patternAssign
patternAssignsParser :: Parser [(Pattern, Expr)]
patternAssignsParser = sepBy1 patternAssignParser (symbol ";")

-- pattern --> expr
patternAssignParser :: Parser (Pattern, Expr)
patternAssignParser = try $ do
    apattern <- patternParser
    symbol "-->"
    expr <- exprParser
    return (apattern, expr)

-- pattern, pattern, ... , pattern
patternsParser :: Parser [Pattern]
patternsParser = sepBy1 patternParser (symbol ",")


patternParser :: Parser Pattern
patternParser = try (PBoolLit True <$ rword "True")
        <|> try (PBoolLit False <$ rword "False")
        <|> try (PIntLit <$> integerParser)
        <|> try (PCharLit <$> charParser)
        <|> try (PVar <$> identifierParser)
        <|> try adtPatternParser
   
-- adt
-- definition: Data adtname := Constructor1 (t1,t2,..,tm) | Constructor2 (t1,t2,..,tn)
adtDefineParser :: Parser ADT
adtDefineParser = try $ do
    rword "data"
    adtname <- identifierParser
    symbol "="
    constructors <- constructorsParser
    return $ ADT adtname constructors

constructorsParser :: Parser [(String, [Type])]
constructorsParser = sepBy1 constructorParser (symbol "|")

constructorParser :: Parser (String, [Type])
constructorParser = try $ do
    funcname <- identifierParser
    symbol "("
    typeList <- typesParser
    symbol ")"
    return (funcname, typeList)

adtPatternParser :: Parser Pattern
adtPatternParser = try $ do
  symbol "["
  constructor <- identifierParser
  symbol "]"
  patterns <- patternsParser
  return $ PData constructor patterns


runAdtDefine :: IO ()
runAdtDefine = do
  input <- getContents
  case runParser adtDefineParser "" input of
    Left error -> print error
    Right a -> print a

runMiniParser :: String -> Either String Expr 
runMiniParser input = 
  case runParser exprParser "" input of
    Left error -> Left $ show error
    Right a -> Right a

run :: IO ()
run = do
  input <- getContents
  case runParser exprParser "" input of
    Left error -> print error
    Right a -> print a

-- examples
-- "\\x::Int -> $x"
-- "letrec Int def inc(x::Int){x+1} in | inc $ 3"
-- "case x+1>2 of True --> False; 3 --> 1; \'A\'-->\'B\'"
-- "data List = Cons (Int->Int, List->(Int->Int)) | Nil ()"


