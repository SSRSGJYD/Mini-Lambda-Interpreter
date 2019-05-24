module ReplContext where

import AST
import Control.Monad.State
import qualified Data.Map as Map
import Util

data ReplContext = ReplContext {
    adtMap :: Map.Map String ADT, -- adtname --> ADT definition
    bindList :: [(String, Expr)] -- 表达式绑定
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
insertBind varname expr context@(ReplContext adtMap bindList) = ReplContext adtMap ((varname, expr):bindList)


showBinds :: [(String, Expr)] -> IO ()
showBinds [] = do
    putStrLn "End of variable bindings."
showBinds (x:xs) = do
    putStrLn $ show (fst x) ++ " := " ++ show (snd x)
    showBinds xs


-- utils
extendBinds :: [(String, Expr)] -> Expr -> Expr
extendBinds [] e = e
extendBinds (x:xs) e = extendBinds xs (ELet x e)