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
} deriving (Show, Eq, Ord)

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

-- expr binding context operations
containExpr :: ContextV -> String -> Bool
containExpr context varname = Map.member varname (exprMap context)

lookupExpr :: ContextV -> String -> Maybe (Expr, ContextV)
lookupExpr context varname = mytrace ("*** lookup expr: " ++ varname ++ " == " ++ show expr) e 
      where e = case Map.lookup varname (exprMap context) of
                  Just ec -> Just $ head ec
                  _ -> Nothing
            expr = case e of
              Just (e,c) -> Just e
              _ -> Nothing

insertExpr :: String -> (Expr, ContextV) -> ContextV -> ContextV
insertExpr varname ec context@(ContextV adtMap constructorMap typeMap exprMap exprRecMap argList log) =  
  if containExpr context varname
  then mytrace ("*** insert expr: " ++ varname ++ " := " ++ (show $ fst ec)) ContextV adtMap constructorMap typeMap (Map.update (\x -> Just (ec : x)) varname exprMap) exprRecMap argList log
  else mytrace ("*** insert expr: " ++ varname ++ " := " ++ (show $ fst ec)) ContextV adtMap constructorMap typeMap (Map.insert varname [ec] exprMap) exprRecMap argList log

deleteExpr :: String -> ContextV -> ContextV
deleteExpr varname context@(ContextV adtMap constructorMap typeMap exprMap exprRecMap argList log) = 
  case Map.lookup varname exprMap of
    Just (t1:t2:t3) -> mytrace ("*** delete expr: " ++ varname) ContextV adtMap constructorMap typeMap (Map.update (\x -> Just (tail x)) varname exprMap) exprRecMap argList log
    Just [t] -> mytrace ("*** delete expr: " ++ varname) ContextV adtMap constructorMap typeMap (Map.delete varname exprMap) exprRecMap argList log
    _ -> mytrace ("*** delete Nothing") context

-- expr binding context operations
containRecExpr :: ContextV -> String -> Bool
containRecExpr context varname = Map.member varname (exprRecMap context)

lookupRecExpr :: ContextV -> String -> Maybe (Expr, ContextV)
lookupRecExpr context varname = mytrace ("*** lookup expr: " ++ varname ++ " == " ++ show expr) e 
      where e = case Map.lookup varname (exprRecMap context) of
                  Just ec -> Just $ head ec
                  _ -> Nothing
            expr = case e of
              Just (e,c) -> Just e
              _ -> Nothing

insertRecExpr :: String -> (Expr, ContextV) -> ContextV -> ContextV
insertRecExpr varname ec context@(ContextV adtMap constructorMap typeMap exprMap exprRecMap argList log) =  
  if containRecExpr context varname
  then mytrace ("*** insert expr: " ++ varname ++ " := " ++ (show $ fst ec)) ContextV adtMap constructorMap typeMap exprMap (Map.update (\x -> Just (ec : x)) varname exprRecMap) argList log
  else mytrace ("*** insert expr: " ++ varname ++ " := " ++ (show $ fst ec)) ContextV adtMap constructorMap typeMap exprMap (Map.insert varname [ec] exprRecMap) argList log

deleteRecExpr :: String -> ContextV -> ContextV
deleteRecExpr varname context@(ContextV adtMap constructorMap typeMap exprMap exprRecMap argList log) = 
  case Map.lookup varname exprRecMap of
    Just (t1:t2:t3) -> mytrace ("*** delete expr: " ++ varname) ContextV adtMap constructorMap typeMap exprMap (Map.update (\x -> Just (tail x)) varname exprRecMap) argList log
    Just [t] -> mytrace ("*** delete expr: " ++ varname) ContextV adtMap constructorMap typeMap exprMap (Map.delete varname exprRecMap) argList log
    _ -> mytrace ("*** delete Nothing") context

-- argument stack for lambda expression
pushArg :: (Expr, ContextV) -> ContextV -> ContextV
pushArg ec (ContextV adtMap constructorMap typeMap exprMap exprRecMap argList log) = mytrace ("### pushing arg:" ++ (show $ fst ec)) ContextV adtMap constructorMap typeMap exprMap exprRecMap (ec:argList) log

pushArgs :: [(Expr, ContextV)] -> ContextV -> ContextV
pushArgs ecs (ContextV adtMap constructorMap typeMap exprMap exprRecMap argList log) = mytrace ("### pushing args") ContextV adtMap constructorMap typeMap exprMap exprRecMap ecs log

emptyArg :: ContextV -> Bool
emptyArg context = null $ argList context

firstArg :: ContextV -> (Expr, ContextV)
firstArg (ContextV adtMap constructorMap typeMap exprMap exprRecMap argList log) = mytrace ("### first arg:" ++ show (fst $ head argList)) head argList

firstArgs :: Int -> ContextV -> [(Expr, ContextV)]
firstArgs n (ContextV adtMap constructorMap typeMap exprMap exprRecMap argList log) = mytrace ("### first" ++ show n ++ "arg:") take n argList

popArg :: ContextV -> ContextV
popArg (ContextV adtMap constructorMap typeMap exprMap exprRecMap argList log) = mytrace ("### poping arg:" ++ show (fst $ head argList)) ContextV adtMap constructorMap typeMap exprMap exprRecMap (tail argList) log

popArgs :: Int -> ContextV -> ContextV
popArgs n (ContextV adtMap constructorMap typeMap exprMap exprRecMap argList log) = mytrace ("### poping args:") ContextV adtMap constructorMap typeMap exprMap exprRecMap (drop n argList) log

countArg :: ContextV -> Int
countArg context = length $ argList context

getAllArgs :: ContextV -> [(Expr, ContextV)]
getAllArgs context = mytrace ("### get all args:") $ argList context

popAll :: ContextV -> ContextV
popAll (ContextV adtMap constructorMap typeMap exprMap exprRecMap argList log) = mytrace ("### poping all args:") ContextV adtMap constructorMap typeMap exprMap exprRecMap [] log

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

                
  