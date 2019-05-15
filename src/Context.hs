module Context where

import AST
import Control.Monad.State
import qualified Data.Map as Map

data Context = Context { -- 可以用某种方式定义上下文，用于记录变量绑定状态
    typeMap :: Map.Map String (Type), -- 类型信,用于EvalType
    exprMap :: Map.Map String (Type, Expr) -- 表达式绑定,用于EvalValue
}
  deriving (Show, Eq)

type ContextState a = StateT Context Maybe a


containType :: Context -> String -> Bool
containType context varname = Map.member varname (typeMap context)

lookupType :: Context -> String -> Maybe Type
lookupType context varname = Map.lookup varname (typeMap context)

insertType :: String -> Type -> Context -> Context
insertType varname mtype (Context typeMap exprMap) = Context (Map.insert varname mtype typeMap) exprMap

deleteType :: String -> Context -> Context
deleteType varname (Context typeMap exprMap) = Context (Map.delete varname typeMap) exprMap

containExpr :: Context -> String -> Bool
containExpr context varname = Map.member varname (exprMap context)
