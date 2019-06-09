module EvalType (evalType) where

import AST
import ContextT
import Control.Monad.State
import qualified Data.Map as Map
import Util


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

isSameType :: Expr -> Expr -> ContextStateT Bool
isSameType e1 e2 = do
  et1 <- EvalType.eval e1
  et2 <- EvalType.eval e2
  return $ et1 == et2

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
eval (EOr e1 e2) = isBool2 e1 e2

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
  t2 <- EvalType.eval e
  modify (deleteType varname)
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
  if et == TArrow argtype returntype
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

-- when function is a variable
eval (EVar varname) = do
  context <- get
  case lookupType context varname of 
    Just t -> return t
    Nothing -> 
      case lookupExpr context varname of 
        Just e ->
            mytrace ("[EVar] EvalType.eval: " ++ show e) EvalType.eval e
        Nothing -> 
          case lookupConstructor context varname of
            Just (adtname, argList) -> return $ evalMultiArgsFuncType argList (TData adtname)
            _ -> lift Nothing
                
eval (EApply e1 e2) = do
  et1 <- mytrace ("[EApply] EvalType.eval: " ++ show e1) EvalType.eval e1
  et2 <- mytrace ("[EApply] EvalType.eval: " ++ show e2) EvalType.eval e2
  case et1 of 
    TArrow t1 t2 -> if et2 == t1 then return t2 else lift Nothing
    _ -> lift Nothing

eval (ECase e list) = do
  context <- get
  et <- EvalType.eval e
  put context
  case list of
    (pe : pes) -> do
        ebool <- matchPatternT (fst pe) et
        if ebool
        then do
            result <- evalPatternType et pe
            evalCasePatternsType et result pes
        else lift Nothing
    _ -> lift Nothing

eval _ = lift Nothing


evalCasePatternsType :: Type -> Type -> [(Pattern, Expr)] -> ContextStateT Type
evalCasePatternsType et expectedType [] = return expectedType
evalCasePatternsType et expectedType (pe : pes) =  do
  ebool <- matchPatternT (fst pe) et
  if ebool
  then do
      result <- evalPatternType et pe
      if result == expectedType
      then evalCasePatternsType et expectedType pes
      else lift Nothing
  else lift Nothing
    

evalPatternType :: Type -> (Pattern, Expr) -> ContextStateT Type
evalPatternType et (p,e) = case p of
  PBoolLit x -> eval e
  PIntLit x -> eval e
  PCharLit x -> eval e
  PVar varname -> do
    modify $ insertType varname et
    result <- eval e
    modify $ deleteType varname
    return result
  PData funcname patterns -> 
    case et of
      TData adtname -> do
        context <- get
        case ContextT.lookupConstructor context funcname of
          Just (adtname', typeList) -> 
            if adtname == adtname'
            then do
              modify $ bindPatternsT patterns typeList
              result <- eval e
              modify $ unbindPatternsT patterns
              return result
            else lift Nothing
          _ -> lift Nothing
      _ -> lift Nothing

matchPatternsT :: [Pattern] -> [Type] -> ContextStateT Bool
matchPatternsT [] [] = return True
matchPatternsT _ [] = return False
matchPatternsT [] _ = return False
matchPatternsT (p:ps) (et:ets) = do
  ebool <- matchPatternT p et
  if ebool
  then matchPatternsT ps ets
  else return False
    

matchPatternT :: Pattern -> Type -> ContextStateT Bool
matchPatternT p et = 
  case p of
    PBoolLit x -> 
      return $ et == TBool
    PIntLit x -> 
      return $ et == TInt
    PCharLit x -> 
      return $ et == TChar
    PVar varname -> return True
    PData funcname patterns -> 
      case et of
        TData adtname -> do
          context <- get
          case ContextT.lookupConstructor context funcname of
            Just (adtname', _) -> return $ adtname == adtname'
            _ -> return False
        _ -> return False


bindPatternsT :: [Pattern] -> [Type] -> ContextT -> ContextT
bindPatternsT (p:ps) (t:ts) context = let context' = bindPatternT p t context
                                      in bindPatternsT ps ts context'
bindPatternsT _ _ context = context


bindPatternT :: Pattern -> Type -> ContextT -> ContextT
bindPatternT p et context = 
  case p of
    PBoolLit x -> context
    PIntLit x -> context
    PCharLit x -> context
    PVar varname -> ContextT.insertType varname et context
    PData funcname [] -> context
    PData constructor patterns -> case et of
      TData adtname -> 
        case ContextT.lookupConstructor context constructor of
          Just (adtname, typeList) -> bindPatternsT patterns typeList context
          _ -> context
      _ -> context


unbindPatternsT :: [Pattern] -> ContextT -> ContextT
unbindPatternsT (p:ps) context = let context' = unbindPatternT p context
                                in unbindPatternsT ps context'
unbindPatternsT _ context = context


unbindPatternT :: Pattern -> ContextT -> ContextT
unbindPatternT p context = 
  case p of
    PBoolLit x -> context
    PIntLit x -> context
    PCharLit x -> context
    PVar varname -> ContextT.deleteType varname context
    PData constructor patterns -> unbindPatternsT patterns context

evalType :: Program -> Maybe Type
evalType (Program adts body) = evalStateT (EvalType.eval body) $
            ContextT { adtMap = initAdtMap adts, 
                      constructorMap = initConstructorMap adts,
                      typeMap = Map.empty, 
                      exprMap = Map.empty,
                      logList = ["start EvalType Program"] }