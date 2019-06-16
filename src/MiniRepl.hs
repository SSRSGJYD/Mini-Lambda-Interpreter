module MiniRepl where

import Text.Megaparsec
import qualified Data.Map as Map
import qualified Data.List.NonEmpty as NE
import Control.Monad.State
import AST
import EvalType
import EvalValue
import MiniParser
import ReplParser
import ReplContext


doEval :: ReplContext -> IO ()
doEval context = do
    putStr "Repl> "
    input <- getLine
    case runParser replParser "" input of
        Left error -> do
            putStrLn "*** MiniRepl Error: parse repl input failed."
            putStr $ errorBundlePretty error
            doEval context
        Right x -> case x of 
            ADTDefine adt@(ADT adtname constructors) -> 
                let context' = insertADT adtname adt context in
                    doEval context'
            JudgeType e -> case judgeType e context of
                Left error -> do
                    putStrLn error
                    doEval context
                Right t -> do
                    print t
                    doEval context
            Bind varname e -> 
                let context' = insertBind varname e context in
                    doEval context'
            BindRec v -> 
                let context' = insertRecBind v context in
                    doEval context'
            Exec e -> case exec e context of
                Left error -> do
                    putStrLn "*** MiniRepl Error: execution failed."
                    putStrLn error
                    doEval context
                Right v -> do
                    print v
                    doEval context
            Binding -> do
                let adts = map snd (Map.toList (adtMap context))
                let binds = bindList context
                showADTs adts
                showBinds binds
                doEval context
            Help -> do
                showHelp
                doEval context
            Quit -> 
                putStrLn "Bye!"
            Block -> do
                input <- readMultiLines ""
                case runParser replParser "" input of
                    Left error -> do
                        putStrLn "*** MiniRepl Error: parse repl input failed."
                        putStr $ errorBundlePretty error
                        doEval context
                    Right x -> case x of 
                        ADTDefine adt@(ADT adtname constructors) -> 
                            let context' = insertADT adtname adt context in
                                doEval context'
                        JudgeType e -> case judgeType e context of
                            Left error -> do
                                putStrLn error
                                doEval context
                            Right t -> do
                                print t
                                doEval context
                        Bind varname e -> 
                            let context' = insertBind varname e context in
                                doEval context'
                        BindRec v -> 
                            let context' = insertRecBind v context in
                                doEval context'
                        Exec e -> case exec e context of
                            Left error -> do
                                putStrLn "*** MiniRepl Error: execution failed."
                                putStrLn error
                                doEval context
                            Right v -> do
                                print v
                                doEval context
                        Binding -> do
                            let adts = map snd (Map.toList (adtMap context))
                            let binds = bindList context
                            showADTs adts
                            showBinds binds
                            doEval context
                        Help -> do
                            showHelp
                            doEval context
                        Quit -> 
                            putStrLn "Bye!"
                        Block -> do
                            putStrLn "*** MiniRepl Error: not support nested block."
                            doEval context


readMultiLines :: String -> IO String
readMultiLines buffer = do
    putStr "Repl| "
    input <- getLine
    let str = [c | c <- input, c /= ' ']
    if str == ":}"
    then return buffer
    else readMultiLines (buffer ++ input)


judgeType :: Expr -> ReplContext -> Either String Type
judgeType e context = 
    let adts = map snd (Map.toList (adtMap context))
        binds = bindList context
        expr = extendBinds binds e
        program = Program adts expr
    in
        case EvalType.evalType program of
            Just t -> Right t
            Nothing -> Left "*** MiniRepl Error: execution failed."

exec :: Expr -> ReplContext -> Either String Result
exec e context = 
    let adts = map snd (Map.toList (adtMap context))
        binds = bindList context
        expr = extendBinds binds e
        program = Program adts expr
    in
        case EvalType.evalType program of
            Just t -> 
                case EvalValue.evalValue program of
                    RInvalid -> Left "*** MiniRepl Error: Invalid expression value."
                    result -> Right result
            Nothing -> Left "*** MiniRepl Error: Invalid expression type."

showHelp :: IO ()
showHelp = do
    putStrLn " Commands available from the prompt:"
    putStr $ "   eval <expr>                  evaluate/run <expr>\n" ++
             "   :{\\n ..lines.. \\n:}\\n        multiline command\n" ++
             "   :h                           display this list of commands\n" ++ 
             "   :quit                        exit MiniRepl\n" ++
             "   :t <expr>                    show the type of <expr>\n" ++
             " -- Commands for displaying information:\n" ++
             "   :bind                        show the current bindings made at the prompt\n"


main :: IO ()
main = do
    putStrLn "Welcome to MiniRepl for MiniLambda language!"
    doEval $ ReplContext {
            adtMap = Map.empty,
            bindList = []
        }

-- examples:
-- True
-- 40+2
-- '@' /= '@'
-- if False then 42 else 233
-- \(x::Int)->x+1
-- let even = (\(x::Int) -> (x % 2) == 0) in even $ 42
-- letrec Int def fact(x::Int){if x == 0 then 1 else x * (fact $ (x-1))} in fact $ 5
