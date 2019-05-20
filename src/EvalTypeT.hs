-- | 这是其中一种实现方式的代码框架。你可以参考它，或用你自己的方式实现，只要按需求完成 evalType :: Program -> Maybe Type 就行。
module EvalType where

import AST
import Context
import Control.Monad.State
import qualified Data.Map as Map
-- import EvalValue
import Util


isBool :: Expr -> ContextState Type
isBool e = do
  et <- EvalTypeT.eval e
  case et of
    TBool -> return TBool
    _ -> lift Nothing

isBool2 :: Expr -> Expr -> ContextState Type
isBool2 e1 e2 = do
  et1 <- EvalTypeT.eval e1
  case et1 of
    TBool -> isBool e2
    _ -> lift Nothing

isInt :: Expr -> ContextState Type
isInt e = do
  et <- EvalTypeT.eval e
  case et of
    TInt -> return TInt
    _ -> lift Nothing

isInt2 :: Expr -> Expr -> ContextState Type
isInt2 e1 e2 = do
  et1 <- EvalTypeT.eval e1
  case et1 of
    TInt -> isInt e2
    _ -> lift Nothing

isChar :: Expr -> ContextState Type
isChar e = do
  et <- EvalTypeT.eval e
  case et of
    TChar -> return TChar
    _ -> lift Nothing

isSameType :: Expr -> Expr -> ContextState Bool
isSameType e1 e2 = do
  et1 <- EvalTypeT.eval e1
  et2 <- EvalTypeT.eval e2
  return $ et1 == et2

isComparableType :: Expr -> ContextState Bool
isComparableType e = do
  et <- EvalTypeT.eval e
  case et of
    TInt -> return True
    TChar -> return True
    _ -> return False

eval :: Expr -> ContextState Type
eval (EBoolLit _) = return TBool
eval (EIntLit _) = return TInt
eval (ECharLit _) = return TChar

eval (ENot e) = isBool e >> return TBool
eval (EAnd e1 e2) = do
  ev1 <- getBool e1
  if not ev1
  then return TBool
  else do
      et2 <- EvalTypeT.eval e2
      if et2 == TBool
      then return TBool
      else lift Nothing

eval (EOr e1 e2) = do
  ev1 <- getBool e1
  if ev1
  then return TBool
  else do
      et2 <- EvalTypeT.eval e2
      if et2 == TBool
      then return TBool
      else lift Nothing

eval (EAdd e1 e2) = isInt2 e1 e2 >> return TInt
eval (ESub e1 e2) = isInt2 e1 e2 >> return TInt
eval (EMul e1 e2) = isInt2 e1 e2 >> return TInt
eval (EDiv e1 e2) = isInt2 e1 e2 >> return TInt
eval (EMod e1 e2) = isInt2 e1 e2 >> return TInt

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
  et <- EvalTypeT.eval e1
  sameType <- EvalType.isSameType e2 e3
  case et of 
    TBool -> if sameType then EvalTypeT.eval e2 else lift Nothing
    _ -> lift Nothing

eval (ELambda (varname, t1) e) = do
  modify (insertType varname t1)
  modify (insertExpr varname e)
  t2 <- EvalTypeT.eval e
  modify (deleteType varname)
  modify (deleteExpr varname)
  return $ TArrow t1 t2

eval (ELet (varname, e1) e2) = do
  modify (insertExpr varname e1)
  t <- mytrace ("[ELet] EvalTypeT.eval: " ++ show e2) EvalTypeT.eval e2
  modify (deleteExpr varname)
  return t

eval ep@(ELetRec funcname (argname,argtype) (funcExpr, returntype) expr) = do
  -- modify (insertExpr funcname (ELambda (argname, argtype) funcExpr))
  -- -- modify (insertType funcname (TArrow argtype returntype))
  -- t <- mytrace ("[ELetRec] EvalTypeT.eval: " ++ show expr) EvalTypeT.eval expr
  -- modify (deleteExpr funcname)
  -- -- modify (deleteType funcname)
  -- return t
  let met = mytrace ("[ELetRec] EvalTypeT.eval: " ++ show (ELambda (argname, argtype) funcExpr)) EvalTypeT.eval $ ELambda (argname, argtype) funcExpr in
    if met == TArrow argtype returntype
    then do
      modify (insertExpr funcname (ELambda (argname, argtype) funcExpr))
      modify (insertType funcname (TArrow argtype returntype))
      t <- mytrace ("[ELetRec] EvalTypeT.eval: " ++ show expr) EvalTypeT.eval expr
      modify (deleteExpr funcname)
      modify (deleteType funcname)
      return t
    else lift Nothing

eval (EVar varname) = do
  context <- get
  case lookupType context varname of 
    Just t -> return t
    Nothing -> do
      case lookupExpr context varname of 
        Just e -> do
            modify (deleteExpr varname)
            et <- mytrace ("[EVar] EvalTypeT.eval: " ++ show e) EvalTypeT.eval e
            modify (insertExpr varname e)
            return et
        Nothing -> case lookupConstructor context varname of
                      Just (adtname, argList) -> return $ evalMultiArgsFuncType argList (TData adtname)
                      _ -> lift Nothing
                
eval (EApply e1 e2) = do
  et1 <- mytrace ("[EApply] EvalTypeT.eval: " ++ show e1) EvalTypeT.eval e1
  et2 <- mytrace ("[EApply] EvalTypeT.eval: " ++ show e2) EvalTypeT.eval e2
  case et1 of 
    TArrow t1 t2 -> if et2 == t1 then return t2 else lift Nothing
    _ -> lift Nothing

eval (ECase e list) = do
  case list of
    (x : xs) -> mytrace ("[ECase] EvalTypeT.eval: " ++ (show $ snd x)) EvalTypeT.eval $ snd x
    _ -> lift Nothing

eval (EConstructor constructor argList) = do
  context <- get
  case lookupConstructor context constructor of
    Just (adtname, argTypes) -> evalApplyMultiArgsFuncType argList $ evalMultiArgsFuncType argTypes (TData adtname)
    _ -> lift Nothing


eval _ = lift Nothing


evalMultiArgsFuncType :: [Type] -> Type -> Type
evalMultiArgsFuncType argTypes returnType = case argTypes of
  [] -> returnType
  (t:ts) -> TArrow t $ evalMultiArgsFuncType ts returnType


evalApplyMultiArgsFuncType :: [Expr] -> Type -> ContextState Type
evalApplyMultiArgsFuncType argTypes funcType = do
  case argTypes of
    [] -> return funcType
    (arg : args) -> case funcType of
        TArrow t1 t2 -> do
          et <- mytrace ("[EConstructor] EvalTypeT.eval: " ++ (show $ arg)) EvalTypeT.eval arg
          if et == t1
          then evalApplyMultiArgsFuncType args t2
          else lift Nothing
        _ -> lift Nothing