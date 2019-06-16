module ReplContext where

import AST
import Control.Monad.State
import qualified Data.Map as Map
import Util

data ReplContext = ReplContext {
    adtMap :: Map.Map String ADT, -- adtname --> ADT definition
    bindList :: [Either (String, Expr) ((String,(String, Type)),(Expr, Type))] -- 表达式绑定
} deriving (Show, Eq)

-- adt definitions
containADT :: ReplContext -> String -> Bool
containADT context varname = Map.member varname (adtMap context)

lookupADT :: ReplContext -> String -> Maybe ADT
lookupADT context varname = Map.lookup varname (adtMap context)

insertADT :: String -> ADT -> ReplContext -> ReplContext
insertADT adtname adt context@(ReplContext adtMap bindList) = 
    if containADT context adtname
    then ReplContext (Map.update (\x -> Just adt) adtname adtMap) bindList
    else ReplContext (Map.insert adtname adt adtMap) bindList

showADTs :: [ADT] -> IO ()
showADTs [] = do
    putStrLn "End of adt definitions."
showADTs (x:xs) = do
    print x
    showADTs xs
    
-- binds
insertBind :: String -> Expr -> ReplContext -> ReplContext
insertBind varname expr context@(ReplContext adtMap bindList) = ReplContext adtMap (Left (varname, expr):bindList)


insertRecBind :: ((String,(String, Type)),(Expr, Type)) -> ReplContext -> ReplContext
insertRecBind recBind context@(ReplContext adtMap bindList) 
    = ReplContext adtMap (Right recBind:bindList)


showBinds :: [Either (String, Expr) ((String,(String, Type)),(Expr, Type))] -> IO ()
showBinds [] = do
    putStrLn "End of variable bindings."
showBinds (x:xs) = case x of 
    Left v -> do
        putStrLn $ show (fst v) ++ " := " ++ show (snd v)
        showBinds xs
    Right ((funcname, (argname, argtype)), (body, returntype)) -> do
        putStrLn $ show returntype ++ " function " ++ show funcname ++ "(" ++ show argname ++ "::" ++ show argtype ++ "){" ++ show body ++ "}"
        showBinds xs

-- utils
extendBinds :: [Either (String, Expr) ((String,(String, Type)),(Expr, Type))] -> Expr -> Expr
extendBinds [] e = e
extendBinds (x:xs) e = case x of
    Left v -> extendBinds xs (ELet v e)
    Right v -> extendBinds xs $ ELetRec funcname (argname, argtype) (body, returntype) e
                    where ((funcname, (argname, argtype)), (body, returntype)) = v