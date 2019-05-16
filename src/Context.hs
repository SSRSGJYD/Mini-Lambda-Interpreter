module Context where

import AST
import Control.Monad.State
import qualified Data.Map as Map

data Context = Context { -- 可以用某种方式定义上下文，用于记录变量绑定状态
    adtMap :: Map.Map String [(String, [Type])], -- 代数数据类型的构造函数
    typeMap :: Map.Map String (Type), -- 类型信息,用于EvalType
    exprMap :: Map.Map String (Expr), -- 表达式绑定,用于EvalValue
    argList :: [Expr] -- 用于处理lambda表达式的未绑定的参数
}
  deriving (Show, Eq)

type ContextState a = StateT Context Maybe a

initAdtMap :: [ADT] -> Map.Map String [(String, [Type])]
initAdtMap [] = Map.empty
imitAdtMap (x:xs) = case x of 
  ADT adtname constructors -> Map.insert adtname constructors $ initAdtMap xs
  _ -> initAdtMap xs

containType :: Context -> String -> Bool
containType context varname = Map.member varname (typeMap context)

lookupType :: Context -> String -> Maybe Type
lookupType context varname = Map.lookup varname (typeMap context)

insertType :: String -> Type -> Context -> Context
insertType varname mtype (Context adtMap typeMap exprMap argList) = Context adtMap (Map.insert varname mtype typeMap) exprMap argList

deleteType :: String -> Context -> Context
deleteType varname (Context adtMap typeMap exprMap argList) = Context adtMap (Map.delete varname typeMap) exprMap argList

containExpr :: Context -> String -> Bool
containExpr context varname = Map.member varname (exprMap context)

lookupExpr :: Context -> String -> Maybe Expr
lookupExpr context varname = Map.lookup varname (exprMap context)

insertExpr :: String -> Expr -> Context -> Context
insertExpr varname expr (Context adtMap typeMap exprMap argList) = Context adtMap typeMap (Map.insert varname expr exprMap) argList

deleteExpr :: String -> Context -> Context
deleteExpr varname (Context adtMap typeMap exprMap argList) = Context adtMap typeMap (Map.delete varname exprMap) argList

-- argument stack for lambda expression
pushArg :: Expr -> Context -> Context
pushArg e (Context adtMap typeMap exprMap argList) = Context adtMap typeMap exprMap $ e:argList

emptyArg :: Context -> Bool
emptyArg (Context adtMap typeMap exprMap argList) = null argList

firstArg :: Context -> Expr
firstArg (Context adtMap typeMap exprMap argList) = head argList

popArg :: Context -> Context
popArg (Context adtMap typeMap exprMap argList) = Context adtMap typeMap exprMap $ tail argList