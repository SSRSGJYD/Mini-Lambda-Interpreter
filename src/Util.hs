module Util where

import AST
import Debug.Trace

mytrace :: String -> a -> a
mytrace = trace -- for debugging
-- mytrace str x = x


wrapValueToExpr :: Value -> Expr
wrapValueToExpr mv = case mv of
    VBool v -> EBoolLit v
    VInt v -> EIntLit v
    VChar v -> ECharLit v
    VData adtname constructor vs -> EConstructor constructor $ map wrapValueToExpr vs

parseValueToResult :: Value -> Result
parseValueToResult v = case v of
    VBool b -> RBool b
    VInt i -> RInt i
    VChar c -> RChar c
    VData adtname constructor argList -> RData adtname constructor $ map parseValueToResult argList
    _ -> RInvalid

parseValueToType :: Value -> Maybe Type
parseValueToType v = case v of
    VBool _ -> Just TBool
    VInt _ -> Just TInt
    VChar _ -> Just TChar
    VData adtname constructor argList -> Just $ TData adtname
    _ -> Nothing

evalLambdaResultType :: Type -> Type
evalLambdaResultType t = case t of
    TArrow t1 t2 -> evalLambdaResultType t2
    t -> t