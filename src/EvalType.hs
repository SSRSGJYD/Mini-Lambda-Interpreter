-- | 这是其中一种实现方式的代码框架。你可以参考它，或用你自己的方式实现，只要按需求完成 evalType :: Program -> Maybe Type 就行。
module EvalType where

import AST
import Context
import Control.Monad.State
import qualified Data.Map as Map
import Debug.Trace


isBool :: Expr -> ContextState Type
isBool e = do
  et <- eval e
  case et of
    TBool -> return TBool
    _ -> lift Nothing

isBool2 :: Expr -> Expr -> ContextState Type
isBool2 e1 e2 = do
  et1 <- eval e1
  case et1 of
    TBool -> isBool e2
    _ -> lift Nothing

isInt :: Expr -> ContextState Type
isInt e = do
  et <- eval e
  case et of
    TInt -> return TInt
    _ -> lift Nothing

isInt2 :: Expr -> Expr -> ContextState Type
isInt2 e1 e2 = do
  et1 <- eval e1
  case et1 of
    TInt -> isInt e2
    _ -> lift Nothing

isChar :: Expr -> ContextState Type
isChar e = do
  et <- eval e
  case et of
    TChar -> return TChar
    _ -> lift Nothing

isSameType :: Expr -> Expr -> ContextState Bool
isSameType e1 e2 = do
  et1 <- eval e1
  et2 <- eval e2
  return $ et1 == et2

isComparableType :: Expr -> ContextState Bool
isComparableType e = do
  et <- eval e
  case et of
    TInt -> return True
    TChar -> return True
    _ -> return False

eval :: Expr -> ContextState Type
eval (EBoolLit _) = return TBool
eval (EIntLit _) = return TInt
eval (ECharLit _) = return TChar

eval (ENot e) = isBool e >> return TBool
eval (EAnd e1 e2) = isBool2 e1 e2 >> return TBool
eval (EOr e1 e2) = isBool2 e1 e2 >> return TBool

eval (EAdd e1 e2) = isInt2 e1 e2 >> return TInt
eval (ESub e1 e2) = isInt2 e1 e2 >> return TInt
eval (EMul e1 e2) = isInt2 e1 e2 >> return TInt
eval (EDiv e1 e2) = isInt2 e1 e2 >> return TInt

eval (EEq e1 e2) = do
  e <- isSameType e1 e2
  if e == True then return TBool else lift Nothing

eval (ENeq e1 e2) = do
  e <- isSameType e1 e2
  if e == True then return TBool else lift Nothing

eval (ELt e1 e2) = do
  sameType <- EvalType.isSameType e1 e2
  comparable <- EvalType.isComparableType e1
  case sameType && comparable of 
    True -> return TBool
    False -> lift Nothing

eval (EGt e1 e2) = do
  sameType <- EvalType.isSameType e1 e2
  comparable <- EvalType.isComparableType e1
  case sameType && comparable of 
    True -> return TBool
    False -> lift Nothing

eval (ELe e1 e2) = do
  sameType <- EvalType.isSameType e1 e2
  comparable <- EvalType.isComparableType e1
  case sameType && comparable of 
    True -> return TBool
    False -> lift Nothing

eval (EGe e1 e2) = do
  sameType <- EvalType.isSameType e1 e2
  comparable <- EvalType.isComparableType e1
  case sameType && comparable of 
    True -> return TBool
    False -> lift Nothing

eval (EIf e1 e2 e3) = do
  et <- eval e1
  sameType <- EvalType.isSameType e2 e3
  case et of 
    TBool -> if sameType then eval e2 else lift Nothing
    _ -> lift Nothing

eval (ELambda (varname, t1) e) = do
  modify (insertType varname t1)
  modify (insertExpr varname e)
  t2 <- eval e
  modify (deleteType varname)
  modify (deleteExpr varname)
  return $ TArrow t1 t2

eval (ELet (varname, e1) e2) = do
  modify (insertExpr varname e1)
  t <- trace ("[ELet] EvalType.eval: " ++ show e2) eval e2
  modify (deleteExpr varname)
  return t

eval (ELetRec funcname (argname,argtype) (funcExpr, returntype) expr) = do
  modify (insertExpr funcname (ELambda (argname, argtype) funcExpr))
  modify (insertType funcname (TArrow argtype returntype))
  t <- trace ("[ELetRec] EvalType.eval: " ++ show expr) eval expr
  modify (deleteExpr funcname)
  modify (deleteType funcname)
  return t

eval (EVar varname) = do
  context <- get
  case lookupType context varname of 
    Just t -> return t
    Nothing -> do
      case lookupExpr context varname of 
        Just e -> do
            modify (deleteExpr varname)
            et <- trace ("[EVar] EvalType.eval: " ++ show e) eval e
            modify (insertExpr varname e)
            return et
        Nothing -> lift Nothing
                

eval (EApply e1 e2) = do
  et1 <- trace ("[EApply] EvalType.eval: " ++ show e1) eval e1
  et2 <- trace ("[EApply] EvalType.eval: " ++ show e2) eval e2
  case et1 of 
    TArrow t1 t2 -> if et2 == t1 then return t2 else lift Nothing
    _ -> lift Nothing

eval (ECase e list) = do
  case list of
    (x : xs) -> trace ("[ECase] EvalType.eval: " ++ (show $ snd x)) eval $ snd x
    _ -> lift Nothing



eval _ = lift Nothing

evalType :: Program -> Maybe Type
evalType (Program adts body) = evalStateT (eval body) $
  Context { adtMap = initAdtMap adts, 
            typeMap = Map.empty, 
            exprMap = Map.empty,
            argList = [],
            logList = ["start EvalType Program"] } -- 可以用某种方式定义上下文，用于记录变量绑定状态
