-- | 这是其中一种实现方式的代码框架。你可以参考它，或用你自己的方式实现，只要按需求完成 evalType :: Program -> Maybe Type 就行。
module EvalType where

import AST
import Control.Monad.State

data Context = Context { -- 可以用某种方式定义上下文，用于记录变量绑定状态
                       }
  deriving (Show, Eq)

type ContextState a = StateT Context Maybe a

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

isSameType :: Expr -> Expr -> Bool
isSameType e1 e2 = (et1 == et2)
  where et1 = eval e1
        et2 = eval e2

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

-- eval (EEq e1 e2) = 

evalType :: Program -> Maybe Type
evalType (Program adts body) = evalStateT (eval body) $
  Context {  } -- 可以用某种方式定义上下文，用于记录变量绑定状态
