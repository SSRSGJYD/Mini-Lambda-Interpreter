module ReplContext where

import AST
import Control.Monad.State
import qualified Data.Map as Map
import Util

data ReplContext = ReplContext {
    adtMap :: Map.Map String ADT, -- adtname --> ADT definition
    bindMap :: Map.Map String Expr -- 表达式绑定
} deriving (Show, Eq)

-- adt definitions
containADT :: ReplContext -> String -> Bool
containADT context varname = Map.member varname (adtMap context)

lookupADT :: ReplContext -> String -> Maybe ADT
lookupADT context varname = Map.lookup varname (adtMap context)

insertADT :: String -> ADT -> ReplContext -> ReplContext
insertADT adtname adt context@(ReplContext adtMap bindMap) = 
    if containADT context adtname
    then ReplContext (Map.update (\x -> Just adt) adtname adtMap) bindMap
    else ReplContext (Map.insert adtname adt adtMap) bindMap

-- binds
containBind :: ReplContext -> String -> Bool
containBind context varname = Map.member varname (bindMap context)

lookupBind :: ReplContext -> String -> Maybe Expr
lookupBind context varname = Map.lookup varname (bindMap context)

insertBind :: String -> Expr -> ReplContext -> ReplContext
insertBind varname expr context@(ReplContext adtMap bindMap) = 
    if containBind context varname
    then ReplContext adtMap (Map.update (\x -> Just expr) varname bindMap)
    else ReplContext adtMap (Map.insert varname expr bindMap)

-- utils
extendBinds :: [(String, Expr)] -> Expr -> Expr
extendBinds [] e = e
extendBinds (x:xs) e = extendBinds xs (ELet x e)