module Util where

import AST
import Debug.Trace

mytrace :: String -> a -> a
-- mytrace = trace -- for debugging
mytrace str x = x

evalMultiArgsFuncType :: [Type] -> Type -> Type
evalMultiArgsFuncType argTypes returnType = case argTypes of
  [] -> returnType
  (t:ts) -> TArrow t $ evalMultiArgsFuncType ts returnType