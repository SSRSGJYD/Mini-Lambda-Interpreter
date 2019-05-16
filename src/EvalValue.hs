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

-- Lambda 表达式无法单独求值

eval (ELet (varname, e1) e2) = do
  modify (insertExpr varname e1)
  t <- EvalValue.eval e2
  modify (deleteExpr varname)
  return t

eval (ELetRec funcname (argname,argtype) (funcExpr, returntype) expr) = do
  modify (insertExpr funcname (ELambda (argname, argtype) funcExpr))
  t <- EvalValue.eval expr
  modify (deleteExpr funcname)
  return t

eval (EVar varname) = do
  context <- get
  case lookupExpr context varname of 
    Just e -> EvalValue.eval e
    Nothing -> lift Nothing

eval (EApply e1 e2) = do
  et1 <- EvalType.eval e1
  et2 <- EvalType.eval e2
  case et1 of 
    TArrow t1 t2 -> if et2 == t1 
                    then
                      case e1 of 
                        ELambda (varname, _) e -> EvalValue.eval $ ELet (varname, e2) e
                        EVar funcname -> do
                          context <- get
                          case lookupExpr context funcname of
                            Just funcexpr -> EvalValue.eval (EApply funcexpr e2)
                            _ -> lift Nothing
                        _ -> lift Nothing
                    else lift Nothing
    _ -> lift Nothing

eval (ECase e list) = do
  case list of
    (p : ps) -> do 
                  ebool <- matchPattern (fst p) e
                  if ebool
                  then do
                    modify (bindPattern (fst p) e)
                    ev <- EvalValue.eval (snd p)
                    modify (unbindPattern (fst p))
                    return ev
                  else
                    EvalValue.eval (ECase e ps)
    _ -> lift Nothing


matchPatterns :: [Pattern] -> [Expr] -> ContextState Bool
matchPatterns [] [] = return True
matchPatterns _ [] = return False
matchPatterns [] _ = return False
matchPatterns (p:ps') (e:es') = do
  ebool <- matchPattern p e
  if ebool
  then matchPatterns ps' es' 
  else return False
    
matchPattern :: Pattern -> Expr -> ContextState Bool
matchPattern p e = do
  ev <- EvalValue.eval e
  case p of
    PBoolLit x -> return $ ev == VBool x
    PIntLit x -> return $ ev == VInt x
    PCharLit x -> return $ ev == VChar x
    PVar varname -> return $ True
    PData constructor [] -> case e of
                              EVar funcname -> return $ constructor == funcname
                              _ -> return False
    PData constructor patterns -> case e of
                        EApply efunc earg -> do 
                                              ebool <- matchPattern (last patterns) earg
                                              if ebool
                                              then matchPattern (PData constructor $ init patterns) efunc
                                              else return False 
                        _ -> return False
    _ -> return False 

bindPattern :: Pattern -> Expr -> Context -> Context
bindPattern p e context = 
  case p of
    PBoolLit x -> context
    PIntLit x -> context
    PCharLit x -> context
    PVar varname -> insertExpr varname e context
    PData constructor [] -> context
    PData constructor patterns -> case e of
      EApply efunc earg -> let context' = bindPattern (last patterns) earg context
                           in bindPattern (PData constructor $ init patterns) efunc context'
      _ -> context
    _ -> context

unbindPattern :: Pattern -> Context -> Context
unbindPattern p context = 
  case p of
    PBoolLit x -> context
    PIntLit x -> context
    PCharLit x -> context
    PVar varname -> deleteExpr varname context
    PData constructor [] -> context
    PData constructor patterns -> let context' = unbindPattern (last patterns) context
                                  in unbindPattern (PData constructor $ init patterns) context'
    _ -> context


evalProgram :: Program -> Maybe Value
evalProgram (Program adts body) = evalStateT (EvalValue.eval body) $
  Context { adtMap = initAdtMap adts, typeMap = Map.empty, exprMap = Map.empty } -- 可以用某种方式定义上下文，用于记录变量绑定状态


evalValue :: Program -> Result
evalValue p = case evalProgram p of
  Just (VBool b) -> RBool b
  Just (VInt i) -> RInt i
  Just (VChar c) -> RChar c
  _ -> RInvalid
