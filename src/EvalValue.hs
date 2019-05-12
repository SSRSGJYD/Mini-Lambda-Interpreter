-- | 这是其中一种实现方式的代码框架。你可以参考它，或用你自己的方式实现，只要按需求完成 evalValue :: Program -> Result 就行。
module EvalValue where

import AST
import Control.Monad.State
import qualified EvalType.eval as evalType

data Value
  = VBool Bool
  | VInt Int
  | VChar Char
  -- ... more
  deriving (Show, Eq)

data Context = Context { -- 可以用某种方式定义上下文，用于记录变量绑定状态
                          } deriving (Show, Eq)

type ContextState a = StateT Context Maybe a

-- for debugging
-- showContextState :: ContextState a -> String
-- showContextState (StateT context) = "context:" ++ show context

getBool :: Expr -> ContextState Bool
getBool e = do
  ev <- eval e
  case ev of
    VBool b -> return b
    _ -> lift Nothing

getInt :: Expr -> ContextState Int
getInt e = do
  ev <- eval e
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

eval (EAdd e1 e2) = getInt e1 >>= \e -> (getInt e2 >>= \f -> return (VInt $ e + f))
eval (ESub e1 e2) = getInt e1 >>= \e -> (getInt e2 >>= \f -> return (VInt $ e - f))
eval (EMul e1 e2) = getInt e1 >>= \e -> (getInt e2 >>= \f -> return (VInt $ e * f))
eval (EDiv e1 e2) = getInt e1 >>= \e -> (getInt e2 >>= \f -> return (VInt $ e `div` f))

eval (EEq e1 e2) = case evalType e1 of 
                      TBool -> getBool e1 >>= \e -> (getBool e2 >>= \f -> return (VBool $ e == f))
                      TInt -> getInt e1 >>= \e -> (getInt e2 >>= \f -> return (VInt $ e == f))
                      TChar -> getChar e1 >>= \e -> (getChar e2 >>= \f -> return (VChar $ e == f))
eval (ENeq e1 e2) = case evalType e1 of 
                      TBool -> getBool e1 >>= \e -> (getBool e2 >>= \f -> return (VBool $ e != f))
                      TInt -> getInt e1 >>= \e -> (getInt e2 >>= \f -> return (VInt $ e != f))
                      TChar -> getChar e1 >>= \e -> (getChar e2 >>= \f -> return (VChar $ e != f))

eval (ELt e1 e2) = case evalType e1 of 
                      TInt -> getInt e1 >>= \e -> (getInt e2 >>= \f -> return (VInt $ e < f))
                      TChar -> getChar e1 >>= \e -> (getChar e2 >>= \f -> return (VChar $ e < f))
eval (EGt e1 e2) = case evalType e1 of 
                      TInt -> getInt e1 >>= \e -> (getInt e2 >>= \f -> return (VInt $ e > f))
                      TChar -> getChar e1 >>= \e -> (getChar e2 >>= \f -> return (VChar $ e > f))
eval (ELe e1 e2) = case evalType e1 of 
                      TInt -> getInt e1 >>= \e -> (getInt e2 >>= \f -> return (VInt $ e <= f))
                      TChar -> getChar e1 >>= \e -> (getChar e2 >>= \f -> return (VChar $ e <= f))
eval (EGe e1 e2) = case evalType e1 of 
                      TInt -> getInt e1 >>= \e -> (getInt e2 >>= \f -> return (VInt $ e >= f))
                      TChar -> getChar e1 >>= \e -> (getChar e2 >>= \f -> return (VChar $ e >= f))

eval (EIf e1 e2 e3) = case eval e1 of
                        VBool True -> eval e2
                        VBool False -> eval e3

evalProgram :: Program -> Maybe Value
evalProgram (Program adts body) = evalStateT (eval body) $
  Context {  } -- 可以用某种方式定义上下文，用于记录变量绑定状态


evalValue :: Program -> Result
evalValue p = case evalProgram p of
  Just (VBool b) -> RBool b
  Just (VInt i) -> RInt i
  Just (VChar c) -> RChar c
  _ -> RInvalid
