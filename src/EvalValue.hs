-- | 这是其中一种实现方式的代码框架。你可以参考它，或用你自己的方式实现，只要按需求完成 evalValue :: Program -> Result 就行。
module EvalValue where

import AST
import Context
import Control.Monad.State
import EvalType
import qualified Data.Map as Map


data Value

  = VBool Bool
  | VInt Int
  | VChar Char
  -- ... more
  deriving (Show, Eq, Ord)


getBool :: Expr -> ContextState Bool
getBool e = do
  ev <- EvalValue.eval e
  case ev of
    VBool b -> return b
    _ -> lift Nothing

getInt :: Expr -> ContextState Int
getInt e = do
  ev <- EvalValue.eval e
  case ev of
    VInt b -> return b
    _ -> lift Nothing

eval :: Expr -> ContextState Value
eval (EBoolLit b) = return $ VBool b
eval (EIntLit i) = return $ VInt i
eval (ECharLit c) = return $ VChar c

eval (ENot e) = getBool e >>= \b -> return (VBool $ not b)
eval (EAnd e1 e2) = getBool e1 >>= \e -> (getBool e2 >>= \f -> return (VBool $ e && f))
eval (EOr e1 e2) = getBool e1 >>= \e -> (getBool e2 >>= \f -> return (VBool $ e || f))

eval (EAdd e1 e2) = do
  ev1 <- getInt e1
  ev2 <- getInt e2
  return $ (VInt $ (ev1 + ev2))

eval (ESub e1 e2) = do
  ev1 <- getInt e1
  ev2 <- getInt e2
  return $ (VInt $ (ev1 - ev2))

eval (EMul e1 e2) = do
  ev1 <- getInt e1
  ev2 <- getInt e2
  return $ (VInt $ (ev1 * ev2))

eval (EDiv e1 e2) = do
  ev1 <- getInt e1
  ev2 <- getInt e2
  return $ (VInt $ (ev1 `div` ev2))

eval (EEq e1 e2) = do
  sameType <- EvalType.isSameType e1 e2
  ev1 <- EvalValue.eval e1
  ev2 <- EvalValue.eval e2
  case sameType of 
    True -> return (VBool $ ev1 == ev2)
    False -> lift Nothing

eval (ENeq e1 e2) = do
  sameType <- EvalType.isSameType e1 e2
  ev1 <- EvalValue.eval e1
  ev2 <- EvalValue.eval e2
  case sameType of 
    True -> return (VBool $ ev1 /= ev2)
    False -> lift Nothing

eval (ELt e1 e2) = do
  sameType <- EvalType.isSameType e1 e2
  comparable <- EvalType.isComparableType e1
  ev1 <- EvalValue.eval e1
  ev2 <- EvalValue.eval e2
  case sameType && comparable of 
    True -> return (VBool $ ev1 < ev2)
    False -> lift Nothing

eval (EGt e1 e2) = do
  sameType <- EvalType.isSameType e1 e2
  comparable <- EvalType.isComparableType e1
  ev1 <- EvalValue.eval e1
  ev2 <- EvalValue.eval e2
  case sameType && comparable of 
    True -> return (VBool $ ev1 > ev2)
    False -> lift Nothing

eval (ELe e1 e2) = do
  sameType <- EvalType.isSameType e1 e2
  comparable <- EvalType.isComparableType e1
  ev1 <- EvalValue.eval e1
  ev2 <- EvalValue.eval e2
  case sameType && comparable of 
    True -> return (VBool $ ev1 <= ev2)
    False -> lift Nothing

eval (EGe e1 e2) = do
  sameType <- EvalType.isSameType e1 e2
  comparable <- EvalType.isComparableType e1
  ev1 <- EvalValue.eval e1
  ev2 <- EvalValue.eval e2
  case sameType && comparable of 
    True -> return (VBool $ ev1 >= ev2)
    False -> lift Nothing
    
eval (EIf e1 e2 e3) = do
  et <- EvalType.eval e1
  sameType <- EvalType.isSameType e2 e3
  case et of
    TBool -> if sameType
             then do 
                ev1 <- EvalValue.eval e1
                case ev1 of
                  VBool True -> EvalValue.eval e2
                  VBool False -> EvalValue.eval e3
             else lift Nothing
    _ -> lift Nothing


evalProgram :: Program -> Maybe Value
evalProgram (Program adts body) = evalStateT (EvalValue.eval body) $
  Context { typeMap = Map.empty, exprMap = Map.empty } -- 可以用某种方式定义上下文，用于记录变量绑定状态


evalValue :: Program -> Result
evalValue p = case evalProgram p of
  Just (VBool b) -> RBool b
  Just (VInt i) -> RInt i
  Just (VChar c) -> RChar c
  _ -> RInvalid
