module ContextV where

import AST
import Control.Monad.State
import qualified Data.Map as Map
import Util

data ContextV = ContextV {
  adtMap :: Map.Map String [(String, [Type])], -- adtname --> [(constructor, argList)]
  constructorMap :: Map.Map String (String, [Type]), -- constructor --> (adtname, argList)
  typeMap :: Map.Map String ([Type]), -- 类型信息,用于EvalType
  exprMap :: Map.Map String ([(Expr, ContextV)]), -- 表达式绑定,用于EvalValue
  exprRecMap :: Map.Map String ([(Expr, ContextV)]), -- 表达式绑定,用于EvalValue
  argList :: [(Expr, ContextV)], -- 用于处理lambda表达式的未绑定的参数
  logList :: [String] -- 执行日志
} deriving (Show, Eq)

type ContextStateV a = StateT ContextV Maybe a

-- ADT
initAdtMap :: [ADT] -> Map.Map String [(String, [Type])]
initAdtMap [] = Map.empty
imitAdtMap (x:xs) = case x of 
  ADT adtname constructors -> Map.insert adtname constructors $ initAdtMap xs
  _ -> initAdtMap xs

initConstructorMap :: [ADT] -> Map.Map String (String, [Type])
initConstructorMap [] = Map.empty
initConstructorMap (x:xs) = case x of 
  ADT adtname constructors -> goConstructors adtname constructors $ initConstructorMap xs
  _ -> initConstructorMap xs

goConstructors :: String -> [(String, [Type])] -> Map.Map String (String, [Type]) -> Map.Map String (String, [Type])
goConstructors adtname constructors map = case constructors of
  [] -> map
  (t:ts) -> Map.insert (fst t) (adtname, (snd t)) $ goConstructors adtname ts map

lookupADT :: ContextV -> String -> Maybe [(String, [Type])]
lookupADT context adtname = Map.lookup adtname $ adtMap context

lookupConstructor :: ContextV -> String -> Maybe (String, [Type])
lookupConstructor context constructor = Map.lookup constructor $ constructorMap context

          
-- type context operations
-- containType :: ContextV -> String -> Bool
-- containType context varname = Map.member varname (typeMap context)

-- lookupType :: ContextV -> String -> Maybe Type
-- lookupType context varname = mytrace2 ("*** lookup type: " ++ varname ++ " == " ++ show e) e
--   where e = case Map.lookup varname (typeMap context) of
--               Just types -> Just $ head types
--               _ -> Nothing

-- insertType :: String -> Type -> ContextV -> ContextV
-- insertType varname mtype context@(ContextV adtMap constructorMap typeMap exprMap argList log) = 
--   if containType context varname
--   then mytrace2 ("*** insert type: " ++ varname ++ " := " ++ show mtype) ContextV adtMap constructorMap (Map.update (\xs -> Just (mtype : xs)) varname typeMap) exprMap argList log
--   else mytrace2 ("*** insert type: " ++ varname ++ " := " ++ show mtype) ContextV adtMap constructorMap (Map.insert varname [mtype] typeMap) exprMap argList log

-- deleteType :: String -> ContextV -> ContextV
-- deleteType varname context@(ContextV adtMap constructorMap typeMap exprMap argList log) = 
--   case Map.lookup varname typeMap of
--     Just (t1:t2:t3) -> mytrace2 ("*** delete type: " ++ varname) ContextV adtMap constructorMap (Map.update (\xs -> Just $ init xs) varname typeMap) exprMap argList log
--     Just [t] -> mytrace2 ("*** delete type: " ++ varname) ContextV adtMap constructorMap (Map.delete varname typeMap) exprMap argList log
--     _ -> mytrace2 ("*** delete Nothing") context

-- expr binding context operations
containExpr :: ContextV -> String -> Bool
containExpr context varname = Map.member varname (exprMap context)

lookupExpr :: ContextV -> String -> Maybe (Expr, ContextV)
lookupExpr context varname = mytrace2 ("*** lookup expr: " ++ varname ++ " == " ++ show expr) e 
      where e = case Map.lookup varname (exprMap context) of
                  Just ec -> Just $ head ec
                  _ -> Nothing
            expr = case e of
              Just (e,c) -> Just e
              _ -> Nothing

insertExpr :: String -> (Expr, ContextV) -> ContextV -> ContextV
insertExpr varname ec context@(ContextV adtMap constructorMap typeMap exprMap exprRecMap argList log) =  
  if containExpr context varname
  then mytrace2 ("*** insert expr: " ++ varname ++ " := " ++ (show $ fst ec)) ContextV adtMap constructorMap typeMap (Map.update (\x -> Just (ec : x)) varname exprMap) exprRecMap argList log
  else mytrace2 ("*** insert expr: " ++ varname ++ " := " ++ (show $ fst ec)) ContextV adtMap constructorMap typeMap (Map.insert varname [ec] exprMap) exprRecMap argList log

deleteExpr :: String -> ContextV -> ContextV
deleteExpr varname context@(ContextV adtMap constructorMap typeMap exprMap exprRecMap argList log) = 
  case Map.lookup varname exprMap of
    Just (t1:t2:t3) -> mytrace2 ("*** delete expr: " ++ varname) ContextV adtMap constructorMap typeMap (Map.update (\x -> Just (tail x)) varname exprMap) exprRecMap argList log
    Just [t] -> mytrace2 ("*** delete expr: " ++ varname) ContextV adtMap constructorMap typeMap (Map.delete varname exprMap) exprRecMap argList log
    _ -> mytrace2 ("*** delete Nothing") context

-- expr binding context operations
containRecExpr :: ContextV -> String -> Bool
containRecExpr context varname = Map.member varname (exprRecMap context)

lookupRecExpr :: ContextV -> String -> Maybe (Expr, ContextV)
lookupRecExpr context varname = mytrace2 ("*** lookup expr: " ++ varname ++ " == " ++ show expr) e 
      where e = case Map.lookup varname (exprRecMap context) of
                  Just ec -> Just $ head ec
                  _ -> Nothing
            expr = case e of
              Just (e,c) -> Just e
              _ -> Nothing

insertRecExpr :: String -> (Expr, ContextV) -> ContextV -> ContextV
insertRecExpr varname ec context@(ContextV adtMap constructorMap typeMap exprMap exprRecMap argList log) =  
  if containRecExpr context varname
  then mytrace2 ("*** insert expr: " ++ varname ++ " := " ++ (show $ fst ec)) ContextV adtMap constructorMap typeMap exprMap (Map.update (\x -> Just (ec : x)) varname exprRecMap) argList log
  else mytrace2 ("*** insert expr: " ++ varname ++ " := " ++ (show $ fst ec)) ContextV adtMap constructorMap typeMap exprMap (Map.insert varname [ec] exprRecMap) argList log

deleteRecExpr :: String -> ContextV -> ContextV
deleteRecExpr varname context@(ContextV adtMap constructorMap typeMap exprMap exprRecMap argList log) = 
  case Map.lookup varname exprRecMap of
    Just (t1:t2:t3) -> mytrace2 ("*** delete expr: " ++ varname) ContextV adtMap constructorMap typeMap exprMap (Map.update (\x -> Just (tail x)) varname exprRecMap) argList log
    Just [t] -> mytrace2 ("*** delete expr: " ++ varname) ContextV adtMap constructorMap typeMap exprMap (Map.delete varname exprRecMap) argList log
    _ -> mytrace2 ("*** delete Nothing") context

-- argument stack for lambda expression
pushArg :: (Expr, ContextV) -> ContextV -> ContextV
pushArg ec (ContextV adtMap constructorMap typeMap exprMap exprRecMap argList log) = mytrace2 ("### pushing arg:" ++ (show $ fst ec)) ContextV adtMap constructorMap typeMap exprMap exprRecMap (ec:argList) log

pushArgs :: [(Expr, ContextV)] -> ContextV -> ContextV
pushArgs ecs (ContextV adtMap constructorMap typeMap exprMap exprRecMap argList log) = mytrace2 ("### pushing args") ContextV adtMap constructorMap typeMap exprMap exprRecMap ecs log

emptyArg :: ContextV -> Bool
emptyArg context = null $ argList context

firstArg :: ContextV -> (Expr, ContextV)
firstArg (ContextV adtMap constructorMap typeMap exprMap exprRecMap argList log) = mytrace2 ("### first arg:" ++ show (fst $ head argList)) head argList

popArg :: ContextV -> ContextV
popArg (ContextV adtMap constructorMap typeMap exprMap exprRecMap argList log) = mytrace2 ("### poping arg:" ++ show (fst $ head argList)) ContextV adtMap constructorMap typeMap exprMap exprRecMap (tail argList) log

countArg :: ContextV -> Int
countArg context = length $ argList context

getAllArgs :: ContextV -> [(Expr, ContextV)]
getAllArgs context = mytrace2 ("### get all args:") $ argList context

popAll :: ContextV -> ContextV
popAll (ContextV adtMap constructorMap typeMap exprMap exprRecMap argList log) = mytrace2 ("### poping all args:") ContextV adtMap constructorMap typeMap exprMap exprRecMap [] log

-- log operations
prependLog :: String -> ContextV -> ContextV
prependLog newLog (ContextV adtMap constructorMap typeMap exprMap exprRecMap argList log) = ContextV adtMap constructorMap typeMap exprMap exprRecMap argList (newLog:log)

printLogs :: ContextV -> IO ()
printLogs (ContextV adtMap constructorMap typeMap exprMap exprRecMap argList log) = printStrLns log


printStrLns :: [String] -> IO()
printStrLns [] = putStrLn "end"
printStrLns (s:ss') = do
                putStrLn s
                printStrLns ss'

                
  