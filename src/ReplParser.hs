module ReplParser where

import AST
import Util
import MiniParser

import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L


data ReplStat = ADTDefine ADT | Bind String Expr | Exec Expr

replParser :: MiniParser.Parser ReplStat
replParser = try bindParser
        <|> try execParser
        <|> try adtParser

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

execParser :: MiniParser.Parser ReplStat
execParser = try $ do
    rword "exec"
    e <- exprParser
    return $ Exec e