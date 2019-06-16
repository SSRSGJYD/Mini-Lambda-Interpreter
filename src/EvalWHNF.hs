module EvalWHNF (eval, Value(..)) where

import AST
import ContextV
import Control.Monad.State
import qualified Data.Map as Map
import Util


data Value
  = VBool Bool
  | VInt Int
  | VChar Char
  | VWHNF String String [Expr] ContextV -- Weak Head Normal Form
  | VData String [Value] -- VData Constructor argList
  deriving (Show, Eq, Ord)

getBool :: Expr -> ContextStateV Bool
getBool e = do
  ev <- EvalWHNF.eval e
  case ev of
    VBool b -> return b
    _ -> lift Nothing

getInt :: Expr -> ContextStateV Int
getInt e = do
  ev <- EvalWHNF.eval e
  case ev of
    VInt b -> return b
    _ -> lift Nothing

eval :: Expr -> ContextStateV Value
eval (EBoolLit b) = return $ VBool b
eval (EIntLit i) = return $ VInt i
eval (ECharLit c) = return $ VChar c

eval (ENot e) = getBool e >>= \b -> return (VBool $ not b)
eval (EAnd e1 e2) = do
  ev1 <- getBool e1
  if not ev1
  then return $ VBool False
  else 
    do
      ev2 <- getBool e2
      return $ VBool ev2

eval (EOr e1 e2) = do
  ev1 <- getBool e1
  if ev1
  then return $ VBool True
  else 
    do
      ev2 <- getBool e2
      return $ VBool ev2

eval (EAdd e1 e2) = do
  ev1 <- getInt e1
  ev2 <- getInt e2
  return (VInt (ev1 + ev2))

eval (ESub e1 e2) = do
  ev1 <- getInt e1
  ev2 <- getInt e2
  return (VInt (ev1 - ev2))

eval (EMul e1 e2) = do
  ev1 <- getInt e1
  ev2 <- getInt e2
  return (VInt (ev1 * ev2))

eval (EDiv e1 e2) = do
  ev1 <- getInt e1
  ev2 <- getInt e2
  if ev2 == 0
  then return $ VInt 0
  else return (VInt (ev1 `div` ev2))

eval (EMod e1 e2) = do
  ev1 <- getInt e1
  ev2 <- getInt e2
  if ev2 == 0
  then return $ VInt 0
  else return (VInt (ev1 `mod` ev2))

eval (EEq e1 e2) = do
  ev1 <- EvalWHNF.eval e1
  ev2 <- EvalWHNF.eval e2
  return (VBool $ ev1 == ev2)

eval (ENeq e1 e2) = do
  ev1 <- EvalWHNF.eval e1
  ev2 <- EvalWHNF.eval e2
  return (VBool $ ev1 /= ev2)

eval (ELt e1 e2) = do
  ev1 <- EvalWHNF.eval e1
  ev2 <- EvalWHNF.eval e2
  return (VBool $ ev1 < ev2)

eval (EGt e1 e2) = do
  ev1 <- EvalWHNF.eval e1
  ev2 <- EvalWHNF.eval e2
  return (VBool $ ev1 > ev2)

eval (ELe e1 e2) = do
  ev1 <- EvalWHNF.eval e1
  ev2 <- EvalWHNF.eval e2
  return (VBool $ ev1 <= ev2)

eval (EGe e1 e2) = do
  ev1 <- EvalWHNF.eval e1
  ev2 <- EvalWHNF.eval e2
  return (VBool $ ev1 >= ev2)
    
eval (EIf e1 e2 e3) = do
  ev1 <- EvalWHNF.eval e1
  case ev1 of
    VBool True -> mytrace ("[if condition true] EvalWHNF.eval: " ++ show e2) EvalWHNF.eval e2
    VBool False -> mytrace ("[if condition false] EvalWHNF.eval: " ++ show e3) EvalWHNF.eval e3

-- Lambda
eval (ELambda (varname, vartype) e) = do
  context <- get
  if emptyArg context
  then mytrace "here" lift Nothing
  else
    let ec = firstArg context in do
      modify popArg
      modify (insertExpr varname ec)
      ev <- mytrace ("[ELambda] EvalWHNF.eval: " ++ show e) EvalWHNF.eval e
      modify (deleteExpr varname)
      return ev

-- let
eval (ELet (varname, e1) e2) = do
  context <- get
  modify (insertExpr varname (e1, context))
  t <- mytrace ("[ELet] EvalWHNF.eval: " ++ show e2) EvalWHNF.eval e2
  modify (deleteExpr varname)
  return t

-- letrec
eval (ELetRec funcname (argname,argtype) (funcExpr, returntype) expr) = do
  context <- get
  modify (insertRecExpr funcname (ELambda (argname, argtype) funcExpr, context))
  t <- mytrace ("[ELetRec] EvalWHNF.eval: " ++ show expr) EvalWHNF.eval expr
  modify (deleteRecExpr funcname)
  return t

-- variable
eval (EVar varname) = do
  context <- get
  case lookupExpr context varname of 
      Just ec -> do
          put $ snd ec
          ev <- mytrace ("[EVar] EvalWHNF.eval: " ++ show (fst ec)) EvalWHNF.eval (fst ec)
          put context
          return ev
      Nothing -> 
        case lookupRecExpr context varname of 
          Just ec -> do
              put $ snd ec
              ev <- mytrace ("[EVar] EvalWHNF.eval: " ++ show (fst ec)) EvalWHNF.eval (fst ec)
              put context
              return ev
          Nothing ->  -- ADT constructor without arguments
              case lookupConstructor context varname of
                  Just (adtname, []) -> 
                    mytrace ("[EVar] EvalWHNF.eval: " ++ show (EConstructor varname [])) EvalWHNF.eval (EConstructor varname [])
                  _ -> lift Nothing
  

-- function apply
eval (EApply e1 e2) = 
  case e1 of 
    ELambda e3@(varname, _) e4 -> do
      context <- get
      modify (pushArg (e2, context))
      mytrace ("[EApply] EvalWHNF.eval: " ++ show e1) EvalWHNF.eval e1
    EApply e3 e4 -> do
      context <- get
      modify (pushArg (e2, context))
      mytrace ("[EApply] EvalWHNF.eval: " ++ show e1) EvalWHNF.eval e1
    EVar funcname -> do
      context <- get
      case lookupExpr context funcname of
        Just ec -> do
          let args = getAllArgs context
          modify popAll
          mytrace ("[EApply] EvalWHNF.eval: " ++ show (EVar funcname)) EvalWHNF.evalVar (EVar funcname) ((e2, context):args)
        _ -> case lookupRecExpr context funcname of
            Just ec -> do
              put $ snd ec
              modify (insertExpr funcname ec)
              put context
              modify (pushArg (e2, context))
              ev <- mytrace ("[E] EvalWHNF.eval: " ++ show (fst ec)) EvalWHNF.eval (fst ec)
              put $ snd ec
              modify (deleteExpr funcname)
              put context
              return ev
            _ -> case lookupConstructor context funcname of
                    Just (adtname, argList) -> do
                      modify (pushArg (e2, context))
                      mytrace ("[EApply] eval: " ++ show (EConstructor funcname [])) eval (EConstructor funcname [])
                    _ -> lift Nothing
    _ -> lift Nothing

-- case
eval (ECase e list) = do
  context <- get
  case list of
    (p : ps) -> do 
      ebool <- matchPattern (fst p) e
      put context
      if ebool
      then do
        bindPattern (fst p) e context
        result <- mytrace ("[ECase] match succeed: " ++ show (fst p)) EvalWHNF.eval (snd p)
        unbindPattern $ fst p
        return result
      else
        mytrace ("[ECase] match failed: " ++ show (fst p)) EvalWHNF.eval (ECase e ps)
    _ -> lift Nothing

-- ADT constructor
eval (EConstructor constructor argList) = do
  context <- get
  case lookupConstructor context constructor of
    Just (adtname, typeList)
      | length argList == length typeList -> 
          return $ VWHNF adtname constructor argList context
      | otherwise -> 
        if emptyArg context
          then lift Nothing
          else
            let ec = firstArg context in do  
              modify popArg
              mytrace ("[EConstructor] EvalWHNF.eval: " ++ show (EConstructor constructor (argList ++ [fst ec]))) EvalWHNF.eval $ EConstructor constructor (argList ++ [fst ec])
              
-- variable with arg
evalVar :: Expr -> [(Expr, ContextV)] -> ContextStateV Value
evalVar (EVar varname) args = do
  context <- get
  case lookupExpr context varname of 
      Just ec ->
          case fst ec of
            EVar varname2 -> do
              put $ snd ec
              ev <- mytrace ("[EVar] EvalWHNF.evalVar: " ++ show (EVar varname2)) EvalWHNF.evalVar (EVar varname2) args
              put context
              return ev
            _ -> do
              put $ snd ec
              modify (pushArgs args)
              ev <- mytrace ("[EVar] EvalWHNF.evalVar: " ++ show (fst ec)) EvalWHNF.eval (fst ec)
              put context
              return ev
      Nothing -> 
        case lookupRecExpr context varname of 
          Just ec -> do
              put $ snd ec
              ev <- mytrace ("[EVar] EvalWHNF.eval: " ++ show (fst ec)) EvalWHNF.eval (fst ec)
              put context
              return ev
          Nothing ->  -- ADT constructor without arguments
              case lookupConstructor context varname of
                  Just (adtname, []) -> 
                    mytrace ("[EVar] EvalWHNF.eval: " ++ show (EConstructor varname [])) EvalWHNF.eval (EConstructor varname [])
                  _ -> lift Nothing


matchPatterns :: [Pattern] -> [Expr] -> ContextStateV Bool
matchPatterns [] [] = return True
matchPatterns _ [] = return False
matchPatterns [] _ = return False
matchPatterns (p:ps) (e:es) = do
  context <- get
  ebool <- matchPattern p e
  put context
  if ebool
  then matchPatterns ps es
  else return False
    
matchPattern :: Pattern -> Expr -> ContextStateV Bool
matchPattern p e =
  case p of
    PBoolLit x -> do
      ev <- mytrace ("match pattern eval: " ++ show e) eval e
      return $ ev == VBool x
    PIntLit x -> do
      ev <- mytrace ("match pattern eval: " ++ show e) eval e
      return $ ev == VInt x
    PCharLit x -> do
      ev <- mytrace ("match pattern eval: " ++ show e) eval e
      return $ ev == VChar x
    PVar varname -> return True
    PData funcname patterns -> do
      ev <- mytrace ("match pattern eval: " ++ show e) eval e
      case ev of
        VWHNF adtname constructor exprList context -> 
          if funcname == constructor
          then do
            put context
            matchPatterns patterns exprList
          else return False
        _ -> return False

bindPatterns :: [Pattern] -> [Expr] -> ContextV -> ContextStateV ()
bindPatterns (p:ps) (e:es) context = do
  bindPattern p e context
  bindPatterns ps es context
  return ()
bindPatterns _ _ _ = return ()

bindPattern :: Pattern -> Expr -> ContextV -> ContextStateV ()
bindPattern p e context = 
  case p of
    PBoolLit x -> return ()
    PIntLit x -> return ()
    PCharLit x -> return ()
    PVar varname -> do
      modify (ContextV.insertExpr varname (e, context))
      return ()
    PData funcname [] -> return ()
    PData constructor patterns -> do
      oldcontext <- get
      ev <- EvalWHNF.eval e
      put oldcontext
      case ev of
        VWHNF adtname constructor exprList context' -> do
          bindPatterns patterns exprList context'
          return ()
        _ -> lift Nothing

unbindPatterns :: [Pattern] -> ContextStateV ()
unbindPatterns (p:ps) = do
  unbindPattern p
  unbindPatterns ps
  return ()
unbindPatterns _ = return ()

unbindPattern :: Pattern -> ContextStateV ()
unbindPattern p = 
  case p of
    PBoolLit x -> return ()
    PIntLit x -> return ()
    PCharLit x -> return ()
    PVar varname -> do
      modify $ ContextV.deleteExpr varname
      return ()
    PData constructor patterns -> 
      unbindPatterns patterns


evalExprList :: [Expr] -> ContextStateV [Value]
evalExprList [] = return []
evalExprList (e:es) = do
  ev <- EvalWHNF.eval e
  esv <- evalExprList es
  return (ev:esv)
