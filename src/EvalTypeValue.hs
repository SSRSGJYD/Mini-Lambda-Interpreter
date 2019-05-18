module EvalTypeValue where

import AST
import Context
import Control.Monad.State
import qualified Data.Map as Map
import Util
import Pattern

isBool :: Expr -> ContextState Type
isBool e = do
  et <- eval e
  case fst et of
    TBool -> return TBool
    _ -> lift Nothing

isBool2 :: Expr -> Expr -> ContextState Type
isBool2 e1 e2 = do
  et1 <- eval e1
  case fst et1 of
    TBool -> isBool e2
    _ -> lift Nothing

isInt :: Expr -> ContextState Type
isInt e = do
  et <- eval e
  case fst et of
    TInt -> return TInt
    _ -> lift Nothing

isInt2 :: Expr -> Expr -> ContextState Type
isInt2 e1 e2 = do
  et1 <- eval e1
  case fst et1 of
    TInt -> isInt e2
    _ -> lift Nothing

isChar :: Expr -> ContextState Type
isChar e = do
  et <- eval e
  case fst et of
    TChar -> return TChar
    _ -> lift Nothing

isSameType :: Expr -> Expr -> ContextState Bool
isSameType e1 e2 = do
  et1 <- eval e1
  et2 <- eval e2
  return $ fst et1 == fst et2

isComparableType :: Type -> Bool
isComparableType t = 
  case t of
    TInt -> True
    TChar -> True
    _ -> False

getBool :: Expr -> ContextState Bool
getBool e = do
  ev <- eval e
  case snd ev of
    VBool b -> b
    _ -> lift Nothing

getInt :: Expr -> ContextState Int
getInt e = do
  ev <- eval e
  case snd ev of
    VInt b -> return b
    _ -> lift Nothing

eval :: Expr -> ContextState (Type, Value)
eval (EBoolLit b) = return (TBool, VBool b)
eval (EIntLit i) = return (TInt, VInt i)
eval (ECharLit c) = return (TChar, VChar c)

eval (ENot e) = getBool e >>= \b -> return (TBool, VBool $ not b)
eval (EAnd e1 e2) = do
  ev1 <- getBool e1
  if not ev1
  then return (TBool, VBool False)
  else 
    do
      ev2 <- getBool e2
      return (TBool, VBool ev2)

eval (EOr e1 e2) = do
  ev1 <- getBool e1
  if ev1
  then return (TBool, VBool True)
  else 
    do
      ev2 <- getBool e2
      return (TBool, VBool ev2)

eval (EAdd e1 e2) = do
  ev1 <- getInt e1
  ev2 <- getInt e2
  return (TInt, VInt $ ev1 + ev2)

eval (ESub e1 e2) = do
  ev1 <- getInt e1
  ev2 <- getInt e2
  return (TInt, VInt $ ev1 - ev2)

eval (EMul e1 e2) = do
  ev1 <- getInt e1
  ev2 <- getInt e2
  return (TInt, VInt $ ev1 * ev2)

eval (EDiv e1 e2) = do
  ev1 <- getInt e1
  ev2 <- getInt e2
  if ev2 == 0
  then return (TInt, VInt 0)
  else return (TInt, VInt $ ev1 `div` ev2)

eval (EMod e1 e2) = do
  ev1 <- getInt e1
  ev2 <- getInt e2
  if ev2 == 0
  then return (TInt, VInt 0)
  else return (TInt, VInt $ ev1 `mod` ev2)

eval (EEq e1 e2) = do
  ev1 <- eval e1
  ev2 <- eval e2
  if fst ev1 == fst ev2
  then return (TBool, VBool $ snd ev1 == snd ev2)
  else lift Nothing

eval (ENeq e1 e2) = do
  ev1 <- eval e1
  ev2 <- eval e2
  if fst ev1 == fst ev2
  then return (TBool, VBool $ snd ev1 /= snd ev2)
  else lift Nothing

eval (ELt e1 e2) = do
  ev1 <- eval e1
  ev2 <- eval e2
  if fst ev1 == fst ev2 && isComparableType (fst ev1)
  then return (TBool, VBool $ snd ev1 < snd ev2)
  else lift Nothing

eval (EGt e1 e2) = do
  ev1 <- eval e1
  ev2 <- eval e2
  if fst ev1 == fst ev2 && isComparableType (fst ev1)
  then return (TBool, VBool $ snd ev1 > snd ev2)
  else lift Nothing

eval (ELe e1 e2) = do
  ev1 <- eval e1
  ev2 <- eval e2
  if fst ev1 == fst ev2 && isComparableType (fst ev1)
  then return (TBool, VBool $ snd ev1 <= snd ev2)
  else lift Nothing

eval (EGe e1 e2) = do
  ev1 <- eval e1
  ev2 <- eval e2
  if fst ev1 == fst ev2 && isComparableType (fst ev1)
  then return (TBool, VBool $ snd ev1 >= snd ev2)
  else lift Nothing
    
eval (EIf e1 e2 e3) = do
  ev1 <- eval e1
  case snd ev1 of
    VBool True -> mytrace ("[if condition true] eval: " ++ show e2) eval e2
    VBool False -> mytrace ("[if condition false] eval: " ++ show e3) eval e3
    _ -> lift Nothing

-- Lambda
eval (ELambda (varname, vartype) e) = do
  context <- get
  if emptyArg context
  then lift Nothing
  else
    let e' = firstArg context in do
      modify popArg
      modify (insertType varname vartype)
      ev <- eval e'
      let e'' = wrapValueToExpr $ snd ev
      result <- mytrace ("[ELambda] eval: " ++ show (ELet (varname, e'') e)) eval $ ELet (varname, e'') e
      modify (deleteType varname)
      return result
      -- return (TArrow vartype $ fst result, snd result)

-- let
eval (ELet (varname, e1) e2) = do
  modify (insertExpr varname e1)
  ev <- mytrace ("[ELet] eval: " ++ show e2) eval e2
  modify (deleteExpr varname)
  return ev

-- letrec
eval (ELetRec funcname (argname,argtype) (funcExpr, returntype) expr) = do
  modify (insertExpr funcname (ELambda (argname, argtype) funcExpr))
  modify (insertType funcname (TArrow argtype returntype))
  t <- mytrace ("[ELetRec] eval: " ++ show expr) eval expr
  modify (deleteExpr funcname)
  modify (deleteType funcname)
  return t

-- variable
eval (EVar varname) = do
  context <- get
  case lookupExpr context varname of 
      Just e -> do
          modify (deleteExpr varname)
          ev <- mytrace ("[EVar] eval: " ++ show e) eval e
          modify (insertExpr varname e)
          case lookupType context varname of
            Just t -> 
                if t == fst ev
                then return ev
                else lift Nothing
            _ -> return ev
      Nothing ->  -- ADT constructor without arguments
          case lookupConstructor context varname of
              Just (adtname, _) ->
                mytrace ("[EVar] eval: " ++ show (EConstructor varname [])) eval (EConstructor varname [])
              _ -> lift Nothing

-- function apply
eval (EApply e1 e2) = 
  case e1 of 
    ELambda e3@(varname, _) e4 -> do
      modify (pushArg e2)
      mytrace ("[EApply] eval: " ++ show (ELambda e3 e4)) eval $ ELambda e3 e4
    EApply e3 e4 -> do
      modify (pushArg e2)
      mytrace ("[EApply] eval: " ++ show e1) eval e1
    EVar funcname -> do
      context <- get
      case lookupExpr context funcname of
        Just e -> do
          ev <- mytrace ("[EApply] eval: " ++ show (EApply e e2)) eval (EApply e e2)
          case lookupType context funcname of
            Just t -> 
                if t == fst ev
                then return ev
                else lift Nothing
            _ -> return ev
        _ -> case lookupConstructor context funcname of
                Just (adtname, argList) -> do
                  modify (pushArg e2)
                  mytrace ("[EApply] eval: " ++ show (EConstructor funcname [])) eval (EConstructor funcname [])
                _ -> lift Nothing
    _ -> lift Nothing

-- case
eval (ECase e list) = do
  ev <- eval e
  case list of
    (p : ps) -> do 
      ebool <- matchPattern (fst p) (snd ev)
      if ebool
      then do
        modify (bindPattern (fst p) (snd ev))
        result <- mytrace ("[ECase] eval: " ++ show (snd p)) eval (snd p)
        modify (unbindPattern (fst p))
        return result
      else
        mytrace ("[ECase] eval: " ++ show (ECase e ps)) eval (ECase e ps)
    _ -> lift Nothing

-- ADT constructor, similar to multi-lambda expression
eval (EConstructor constructor argList) = do
  context <- get
  case lookupConstructor context constructor of
    Just (adtname, typeList) -> 
      if length argList == length typeList
      then do
        evs <- mytrace ("[EConstructor] evalExprList: " ++ show argList) evalExprList argList
        return (TData adtname, VData adtname constructor $ evs)
      else 
        if emptyArg context
          then lift Nothing
          else
            let e = firstArg context in do  
              modify popArg
              ev <- eval e
              let e' = wrapValueToExpr $ snd ev
              mytrace ("[EConstructor] eval: " ++ show (EConstructor constructor (argList ++ [e']))) eval $ EConstructor constructor (argList ++ [e'])
              

evalExprList :: [Expr] -> ContextState [Value]
evalExprList [] = return []
evalExprList (e:es) = do
  ev <- eval e
  esv <- evalExprList es
  return $ snd ev:esv