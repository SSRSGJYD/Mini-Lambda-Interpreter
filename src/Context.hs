module Context where

import AST
import Control.Monad.State
import qualified Data.Map as Map
import Debug.Trace

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
  adtMap :: Map.Map String [(String, [Type])], -- 代数数据类型的构造函数
  typeMap :: Map.Map String ([Type]), -- 类型信息,用于EvalType
  exprMap :: Map.Map String ([Expr]), -- 表达式绑定,用于EvalValue
  argList :: [Expr], -- 用于处理lambda表达式的未绑定的参数
  logList :: [String] -- 执行日志
} deriving (Show, Eq)

type ContextState a = StateT Context Maybe a

initAdtMap :: [ADT] -> Map.Map String [(String, [Type])]
initAdtMap [] = Map.empty
imitAdtMap (x:xs) = case x of 
  ADT adtname constructors -> Map.insert adtname constructors $ initAdtMap xs
  _ -> initAdtMap xs

containType :: Context -> String -> Bool
containType context varname = Map.member varname (typeMap context)

lookupType :: Context -> String -> Maybe Type
lookupType context varname = case Map.lookup varname (typeMap context) of
  Just types -> Just $ head types
  _ -> Nothing

insertType :: String -> Type -> Context -> Context
insertType varname mtype context@(Context adtMap typeMap exprMap argList log) = 
  if containType context varname
  then Context adtMap (Map.update (\xs -> Just (mtype : xs)) varname typeMap) exprMap argList log
  else Context adtMap (Map.insert varname [mtype] typeMap) exprMap argList log

deleteType :: String -> Context -> Context
deleteType varname context@(Context adtMap typeMap exprMap argList log) = 
  case Map.lookup varname typeMap of
    Just (t:ts) -> Context adtMap (Map.update (\xs -> Just $ init xs) varname typeMap) exprMap argList log
    Just [t] -> Context adtMap (Map.delete varname typeMap) exprMap argList log

containExpr :: Context -> String -> Bool
containExpr context varname = Map.member varname (exprMap context)

lookupExpr :: Context -> String -> Maybe Expr
lookupExpr context varname = trace ("lookup expr: " ++ varname ++ " == " ++ (show e)) e 
      where e = case Map.lookup varname (exprMap context) of
                  Just exprs -> Just $ head exprs
                  _ -> Nothing

insertExpr :: String -> Expr -> Context -> Context
insertExpr varname expr context@(Context adtMap typeMap exprMap argList log) =  
  if containExpr context varname
  then trace ("insert expr: " ++ varname ++ " := " ++ (show expr)) Context adtMap typeMap (Map.update (\x -> Just (expr : x)) varname exprMap) argList log
  else trace ("insert expr: " ++ varname ++ " := " ++ (show expr)) Context adtMap typeMap (Map.insert varname [expr] exprMap) argList log

deleteExpr :: String -> Context -> Context
deleteExpr varname context@(Context adtMap typeMap exprMap argList log) = 
  case Map.lookup varname exprMap of
    Just (t:t2:t3) -> trace ("delete expr: " ++ varname) Context adtMap typeMap (Map.update (\x -> Just (tail x)) varname exprMap) argList log
    Just [t] -> trace ("delete expr: " ++ varname) Context adtMap typeMap (Map.delete varname exprMap) argList log
    _ -> trace ("delete Nothing") context

-- argument stack for lambda expression
pushArg :: Expr -> Context -> Context
pushArg e (Context adtMap typeMap exprMap argList log) = trace ("pushing expr:" ++ (show e)) Context adtMap typeMap exprMap (e:argList) log

emptyArg :: Context -> Bool
emptyArg (Context adtMap typeMap exprMap argList log) = null argList

firstArg :: Context -> Expr
firstArg (Context adtMap typeMap exprMap argList log) = head argList

popArg :: Context -> Context
popArg (Context adtMap typeMap exprMap argList log) = Context adtMap typeMap exprMap (tail argList) log

-- log operations
prependLog :: String -> Context -> Context
prependLog newLog (Context adtMap typeMap exprMap argList log) = Context adtMap typeMap exprMap argList (newLog:log)

printLogs :: Context -> IO ()
printLogs (Context adtMap typeMap exprMap argList log) = printStrLns log


printStrLns :: [String] -> IO()
printStrLns [] = putStrLn "end"
printStrLns (s:ss') = do
                putStrLn s
                printStrLns ss'
  
  