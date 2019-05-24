module MiniRepl where

import Text.Megaparsec
import qualified Data.Map as Map
import Control.Monad.State
import AST
import EvalType
import EvalValue
import MiniParser
import ReplParser
import ReplContext

doEval :: ReplContext -> IO ()
doEval context = do
    putStr "MiniRepl> "
    input <- getLine
    case runParser replParser "" input of
        Left error -> print "*** invalid repl grammar"
        Right x -> case x of 
            ADTDefine adt@(ADT adtname constructors) -> 
                let context' = insertADT adtname adt context in
                    doEval context'
            Bind varname e -> 
                let context' = insertBind varname e context in
                    doEval context'
            Exec e -> case exec e context of
                Left error -> print error
                Right v -> print v

exec :: Expr -> ReplContext -> Either String Result
exec e context = 
    let adts = map snd (Map.toList (adtMap context))
        binds = Map.toList (bindMap context)
        expr = extendBinds binds e
        program = Program adts expr
    in
        case EvalType.evalType program of
            Just t -> 
                case EvalValue.evalValue program of
                    RInvalid -> Left "Invalid value, execution failed"
                    result -> Right result
            Nothing -> Left "Type check failed"

repl :: IO ()
repl = doEval $ ReplContext {
            adtMap = Map.empty,
            bindMap = Map.empty
        }