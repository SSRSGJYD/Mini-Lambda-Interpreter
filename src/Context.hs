module Context where

import AST
import Control.Monad.State
import qualified Data.Map as Map
import Util

-- data StackFrame = StackFrame {
--   adtMap :: Map.Map String [(String, [Type])], -- 代数数据类型的构造函数
--   typeMap :: Map.Map String (Type), -- 类型信息,用于EvalType
--   exprMap :: Map.Map String (Expr), -- 表达式绑定,用于EvalValue
--   argList :: [Expr], -- 用于处理lambda表达式的未绑定的参数
--   logList :: [String] -- 执行日志
-- } deriving (Show, Eq)

-- data Context = Context { -- 可以用某种方式定义上下文，用于记录变量绑定状态
--     stack :: [StackFrame]
-- }
--   deriving (Show, Eq)

-- pushStack :: Context -> Context
-- pushStack stacks = Context (StackFrame { adtMap = initAdtMap adts, 
--                                           typeMap = Map.empty, 
--                                           exprMap = Map.empty,
--                                           argList = [],
--                                           logList = ["push a new stack frame"] }) : stacks

-- popStack :: Context -> Context
-- pushStack (s:stacks) = stacks


data Context = Context {
  adtMap :: Map.Map String [(String, [Type])], -- adtname --> [(constructor, argList)]
  constructorMap :: Map.Map String (String, [Type]), -- constructor --> (adtname, argList)
  typeMap :: Map.Map String ([Type]), -- 类型信息,用于EvalType
  exprMap :: Map.Map String ([Expr]), -- 表达式绑定,用于EvalValue
  argList :: [Expr], -- 用于处理lambda表达式的未绑定的参数
  logList :: [String] -- 执行日志
} deriving (Show, Eq)

type ContextState a = StateT Context Maybe a

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

lookupADT :: Context -> String -> Maybe [(String, [Type])]
lookupADT context adtname = Map.lookup adtname $ adtMap context

lookupConstructor :: Context -> String -> Maybe (String, [Type])
lookupConstructor context constructor = Map.lookup constructor $ constructorMap context

          
-- type context operations
containType :: Context -> String -> Bool
containType context varname = Map.member varname (typeMap context)

lookupType :: Context -> String -> Maybe Type
lookupType context varname = mytrace ("*** lookup type: " ++ varname ++ " == " ++ show e) e
  where e = case Map.lookup varname (typeMap context) of
              Just types -> Just $ head types
              _ -> Nothing

insertType :: String -> Type -> Context -> Context
insertType varname mtype context@(Context adtMap constructorMap typeMap exprMap argList log) = 
  if containType context varname
  then mytrace ("*** insert type: " ++ varname ++ " := " ++ show mtype) Context adtMap constructorMap (Map.update (\xs -> Just (mtype : xs)) varname typeMap) exprMap argList log
  else mytrace ("*** insert type: " ++ varname ++ " := " ++ show mtype) Context adtMap constructorMap (Map.insert varname [mtype] typeMap) exprMap argList log

deleteType :: String -> Context -> Context
deleteType varname context@(Context adtMap constructorMap typeMap exprMap argList log) = 
  case Map.lookup varname typeMap of
    Just (t1:t2:t3) -> mytrace ("*** delete type: " ++ varname) Context adtMap constructorMap (Map.update (\xs -> Just $ init xs) varname typeMap) exprMap argList log
    Just [t] -> mytrace ("*** delete type: " ++ varname) Context adtMap constructorMap (Map.delete varname typeMap) exprMap argList log
    _ -> mytrace ("*** delete Nothing") context

-- expr binding context operations
containExpr :: Context -> String -> Bool
containExpr context varname = Map.member varname (exprMap context)

lookupExpr :: Context -> String -> Maybe Expr
lookupExpr context varname = mytrace ("*** lookup expr: " ++ varname ++ " == " ++ (show e)) e 
      where e = case Map.lookup varname (exprMap context) of
                  Just exprs -> Just $ head exprs
                  _ -> Nothing

insertExpr :: String -> Expr -> Context -> Context
insertExpr varname expr context@(Context adtMap constructorMap typeMap exprMap argList log) =  
  if containExpr context varname
  then mytrace ("*** insert expr: " ++ varname ++ " := " ++ (show expr)) Context adtMap constructorMap typeMap (Map.update (\x -> Just (expr : x)) varname exprMap) argList log
  else mytrace ("*** insert expr: " ++ varname ++ " := " ++ (show expr)) Context adtMap constructorMap typeMap (Map.insert varname [expr] exprMap) argList log

deleteExpr :: String -> Context -> Context
deleteExpr varname context@(Context adtMap constructorMap typeMap exprMap argList log) = 
  case Map.lookup varname exprMap of
    Just (t1:t2:t3) -> mytrace ("*** delete expr: " ++ varname) Context adtMap constructorMap typeMap (Map.update (\x -> Just (tail x)) varname exprMap) argList log
    Just [t] -> mytrace ("*** delete expr: " ++ varname) Context adtMap constructorMap typeMap (Map.delete varname exprMap) argList log
    _ -> mytrace ("*** delete Nothing") context

-- argument stack for lambda expression
pushArg :: Expr -> Context -> Context
pushArg e (Context adtMap constructorMap typeMap exprMap argList log) = mytrace ("### pushing arg:" ++ (show e)) Context adtMap constructorMap typeMap exprMap (e:argList) log

emptyArg :: Context -> Bool
emptyArg (Context adtMap constructorMap typeMap exprMap argList log) = null argList

firstArg :: Context -> Expr
firstArg (Context adtMap constructorMap typeMap exprMap argList log) = mytrace ("### first arg:" ++ (show $ head argList)) head argList

popArg :: Context -> Context
popArg (Context adtMap constructorMap typeMap exprMap argList log) = mytrace ("### poping arg:" ++ show (head argList)) Context adtMap constructorMap typeMap exprMap (tail argList) log

countArg :: Context -> Int
countArg context = length $ argList context


-- log operations
prependLog :: String -> Context -> Context
prependLog newLog (Context adtMap constructorMap typeMap exprMap argList log) = Context adtMap constructorMap typeMap exprMap argList (newLog:log)

printLogs :: Context -> IO ()
printLogs (Context adtMap constructorMap typeMap exprMap argList log) = printStrLns log


printStrLns :: [String] -> IO()
printStrLns [] = putStrLn "end"
printStrLns (s:ss') = do
                putStrLn s
                printStrLns ss'

                
  