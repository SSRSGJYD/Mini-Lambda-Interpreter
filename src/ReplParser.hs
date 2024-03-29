module ReplParser (ReplStat(..), replParser) where

import AST
import Util
import MiniParser

import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L


data ReplStat = ADTDefine ADT 
    | JudgeType Expr 
    | Bind String Expr 
    | BindRec ((String, (String, Type)), (Expr, Type))
    | Exec Expr 
    | Block
    | Binding
    | Help
    | Quit

replParser :: MiniParser.Parser ReplStat
replParser = try bindParser
        <|> try bindRecParser
        <|> try execParser
        <|> try evalTypeParser
        <|> try adtParser
        <|> try blockParser
        <|> try showBindParser
        <|> try helpParser
        <|> try quitParser

adtParser :: MiniParser.Parser ReplStat
adtParser = try $ do
    adt <- adtDefineParser
    return $ ADTDefine adt

bindParser :: MiniParser.Parser ReplStat
bindParser = try $ do
    rword "let"
    varname <- identifierParser
    symbol "="
    e <- exprParser
    return $ Bind varname e

bindRecParser :: MiniParser.Parser ReplStat
bindRecParser = try $ do
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
    return $ BindRec ((funcname, (argname, argtype)),(funcbody, returntype))

evalTypeParser :: MiniParser.Parser ReplStat
evalTypeParser = try $ do
    rword ":t"
    e <- exprParser
    return $ JudgeType e


execParser :: MiniParser.Parser ReplStat
execParser = try $ do
    rword "eval"
    e <- exprParser
    return $ Exec e

blockParser :: MiniParser.Parser ReplStat
blockParser = try $ do
    rword ":{"
    return Block

showBindParser :: MiniParser.Parser ReplStat
showBindParser = try $ do
    rword ":bind"
    return Binding

helpParser :: MiniParser.Parser ReplStat
helpParser = try $ do
    rword ":help"
    return Help

quitParser :: MiniParser.Parser ReplStat
quitParser = try $ do
    rword ":quit"
    return Quit

