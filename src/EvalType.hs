module EvalType where

import AST
import ContextT
import Control.Monad.State
import qualified Data.Map as Map
import Util
import Pattern


isBool :: Expr -> ContextStateT Type
isBool e = do
  et <- EvalType.eval e
  case et of
    TBool -> return TBool
    _ -> lift Nothing

isBool2 :: Expr -> Expr -> ContextStateT Type
isBool2 e1 e2 = do
  et1 <- EvalType.eval e1
  case et1 of
    TBool -> isBool e2
    _ -> lift Nothing

isInt :: Expr -> ContextStateT Type
isInt e = do
  et <- EvalType.eval e
  case et of
    TInt -> return TInt
    _ -> lift Nothing

isInt2 :: Expr -> Expr -> ContextStateT Type
isInt2 e1 e2 = do
  et1 <- EvalType.eval e1
  case et1 of
    TInt -> isInt e2
    _ -> lift Nothing

isChar :: Expr -> ContextStateT Type
isChar e = do
  et <- EvalType.eval e
  case et of
    TChar -> return TChar
    _ -> lift Nothing

isSameType :: Expr -> Expr -> ContextStateT Bool
isSameType e1 e2 = do
  et1 <- EvalType.eval e1
  et2 <- EvalType.eval e2
  return $ eqType et1 et2

eqType :: Type -> Type -> Bool
eqType t1 t2 = formatType t1 == formatType t2

isComparableType :: Expr -> ContextStateT Bool
isComparableType e = do
  et <- EvalType.eval e
  case et of
    TInt -> return True
    TChar -> return True
    _ -> return False

eval :: Expr -> ContextStateT Type
eval (EBoolLit _) = return TBool
eval (EIntLit _) = return TInt
eval (ECharLit _) = return TChar

eval (ENot e) = isBool e >> return TBool
eval (EAnd e1 e2) = isBool2 e1 e2
  -- ev1 <- getBool e1
  -- if not ev1
  -- then return TBool
  -- else do
  --     et2 <- EvalType.eval e2
  --     if et2 == TBool
  --     then return TBool
  --     else lift Nothing

eval (EOr e1 e2) = isBool2 e1 e2
  -- ev1 <- getBool e1
  -- if ev1
  -- then return TBool
  -- else do
  --     et2 <- EvalType.eval e2
  --     if et2 == TBool
  --     then return TBool
  --     else lift Nothing

eval (EAdd e1 e2) = isInt2 e1 e2 >> return TInt
eval (ESub e1 e2) = isInt2 e1 e2 >> return TInt
eval (EMul e1 e2) = isInt2 e1 e2 >> return TInt
eval (EDiv e1 e2) = isInt2 e1 e2 >> return TInt
eval (EMod e1 e2) = isInt2 e1 e2 >> return TInt

eval (EEq e1 e2) = do
  e <- isSameType e1 e2
  if e then return TBool else lift Nothing

eval (ENeq e1 e2) = do
  e <- isSameType e1 e2
  if e then return TBool else lift Nothing

eval (ELt e1 e2) = do
  sameType <- EvalType.isSameType e1 e2
  comparable <- EvalType.isComparableType e1
  if sameType && comparable 
  then return TBool
  else lift Nothing

eval (EGt e1 e2) = do
  sameType <- EvalType.isSameType e1 e2
  comparable <- EvalType.isComparableType e1
  if sameType && comparable 
  then return TBool
  else lift Nothing

eval (ELe e1 e2) = do
  sameType <- EvalType.isSameType e1 e2
  comparable <- EvalType.isComparableType e1
  if sameType && comparable 
  then return TBool
  else lift Nothing

eval (EGe e1 e2) = do
  sameType <- EvalType.isSameType e1 e2
  comparable <- EvalType.isComparableType e1
  if sameType && comparable 
  then return TBool
  else lift Nothing

eval (EIf e1 e2 e3) = do
  et1 <- EvalType.eval e1
  case et1 of 
    TBool -> do
      sameType <- EvalType.isSameType e2 e3
      if sameType 
      then EvalType.eval e2 
      else lift Nothing
    _ -> lift Nothing

eval (ELambda (varname, t1) e) = do
  modify (insertType varname t1)
  -- modify (insertExpr varname e)
  t2 <- EvalType.eval e
  modify (deleteType varname)
  -- modify (deleteExpr varname)
  return $ TArrow t1 t2

eval (ELet (varname, e1) e2) = do
  modify (insertExpr varname e1)
  t <- mytrace ("[ELet] EvalType.eval: " ++ show e2) EvalType.eval e2
  modify (deleteExpr varname)
  return t

eval ep@(ELetRec funcname (argname,argtype) (funcExpr, returntype) expr) = do
  modify (insertExpr funcname (ELambda (argname, argtype) funcExpr))
  modify (insertType funcname (TArrow argtype returntype))
  et <- mytrace ("[ELetRec] EvalType.eval: " ++ show (ELambda (argname, argtype) funcExpr)) EvalType.eval $ ELambda (argname, argtype) funcExpr
  if eqType et $ TArrow argtype returntype
  then do
    modify (deleteType funcname)
    modify (insertType funcname et)
    t <- mytrace ("[ELetRec] EvalType.eval: " ++ show expr) EvalType.eval expr
    modify (deleteExpr funcname)
    modify (deleteType funcname)
    return t
  else do
    modify (deleteExpr funcname)
    modify (deleteType funcname)
    lift Nothing


eval (EVar varname) = do
  context <- get
  case lookupType context varname of 
    Just t -> return t
    Nothing -> 
      case lookupExpr context varname of 
        Just e -> do
            -- modify (deleteExpr varname)
            et <- mytrace ("[EVar] EvalType.eval: " ++ show e) EvalType.eval e
            -- modify (insertExpr varname e)
            return et
        Nothing -> 
          case lookupConstructor context varname of
            Just (adtname, argList) -> return $ evalMultiArgsFuncType argList (TConstructor adtname varname argList)
            _ -> lift Nothing
                
eval (EApply e1 e2) = do
  et1 <- mytrace ("[EApply] EvalType.eval: " ++ show e1) EvalType.eval e1
  et2 <- mytrace ("[EApply] EvalType.eval: " ++ show e2) EvalType.eval e2
  case et1 of 
    TArrow t1 t2 -> if eqType et2 t1 then return t2 else lift Nothing
    _ -> lift Nothing

eval (ECase e list) = do
  et <- EvalType.eval e
  case list of
    (p : ps) -> do 
      ebool <- matchPatternT (fst p) et
      if ebool
      then do
        modify (bindPatternT (fst p) et)
        result <- mytrace ("[ECase] EvalType.eval: " ++ show (snd p)) EvalType.eval (snd p)
        modify (unbindPatternT (fst p))
        return result
      else
        mytrace ("[ECase] EvalType.eval: " ++ show (ECase e ps)) EvalType.eval (ECase e ps)
    _ -> lift Nothing



eval _ = lift Nothing


evalMultiArgsFuncType :: [Type] -> Type -> Type
evalMultiArgsFuncType argTypes returnType = case argTypes of
  [] -> returnType
  (t:ts) -> TArrow t $ evalMultiArgsFuncType ts returnType


evalApplyMultiArgsFuncType :: [Expr] -> Type -> ContextStateT Type
evalApplyMultiArgsFuncType argTypes funcType = 
  case argTypes of
    [] -> return funcType
    (arg : args) -> case funcType of
        TArrow t1 t2 -> do
          et <- mytrace ("[EConstructor] EvalType.eval: " ++ show arg) EvalType.eval arg
          if eqType et t1
          then evalApplyMultiArgsFuncType args t2
          else lift Nothing
        _ -> lift Nothing


evalType :: Program -> Maybe Type
evalType (Program adts body) = 
  let mt = evalStateT (EvalType.eval body) $
            ContextT { adtMap = initAdtMap adts, 
                      constructorMap = initConstructorMap adts,
                      typeMap = Map.empty, 
                      exprMap = Map.empty,
                      logList = ["start EvalType Program"] }
  in case mt of
    Just t -> Just $ formatType t
    _ -> Nothing