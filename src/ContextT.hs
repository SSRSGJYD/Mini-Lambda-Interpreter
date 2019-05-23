module ContextT where

  import AST
  import Control.Monad.State
  import qualified Data.Map as Map
  import Util
  
  data ContextT = ContextT {
    adtMap :: Map.Map String [(String, [Type])], -- adtname --> [(constructor, )]
    constructorMap :: Map.Map String (String, [Type]), -- constructor --> (adtname, )
    typeMap :: Map.Map String ([Type]), -- 类型信息,用于EvalType
    exprMap :: Map.Map String ([(Expr)]), -- 表达式绑定,用于EvalValue
    logList :: [String] -- 执行日志
  } deriving (Show, Eq)
  
  type ContextStateT a = StateT ContextT Maybe a
  
  -- ADT
  initAdtMap :: [ADT] -> Map.Map String [(String, [Type])]
  initAdtMap [] = Map.empty
  imitAdtMap (x:xs) = case x of 
    ADT adtname constructors -> Map.insert adtname constructors $ initAdtMap xs
    _ -> initAdtMap xs
  
  initConstructorMap :: [ADT] -> Map.Map String (String, [Type])
  initConstructorMap [] = Map.empty
  initConstructorMap (x:xs) = case x of 
    ADT adtname constructors -> goConstructors adtname constructors $ initConstructorMap xs
    _ -> initConstructorMap xs
  
  goConstructors :: String -> [(String, [Type])] -> Map.Map String (String, [Type]) -> Map.Map String (String, [Type])
  goConstructors adtname constructors map = case constructors of
    [] -> map
    (t:ts) -> Map.insert (fst t) (adtname, (snd t)) $ goConstructors adtname ts map
  
  lookupADT :: ContextT -> String -> Maybe [(String, [Type])]
  lookupADT context adtname = Map.lookup adtname $ adtMap context
  
  lookupConstructor :: ContextT -> String -> Maybe (String, [Type])
  lookupConstructor context constructor = mytrace2 ("*** lookup constructor: " ++ show constructor) Map.lookup constructor $ constructorMap context
  
            
  -- type context operations
  containType :: ContextT -> String -> Bool
  containType context varname = Map.member varname (typeMap context)
  
  lookupType :: ContextT -> String -> Maybe Type
  lookupType context varname = mytrace ("*** lookup type: " ++ varname ++ " == " ++ show e) e
    where e = case Map.lookup varname (typeMap context) of
                Just types -> Just $ head types
                _ -> Nothing
  
  insertType :: String -> Type -> ContextT -> ContextT
  insertType varname mtype context@(ContextT adtMap constructorMap typeMap exprMap log) = 
    if containType context varname
    then mytrace ("*** insert type: " ++ varname ++ " := " ++ show mtype) ContextT adtMap constructorMap (Map.update (\xs -> Just (mtype : xs)) varname typeMap) exprMap log
    else mytrace ("*** insert type: " ++ varname ++ " := " ++ show mtype) ContextT adtMap constructorMap (Map.insert varname [mtype] typeMap) exprMap log
  
  deleteType :: String -> ContextT -> ContextT
  deleteType varname context@(ContextT adtMap constructorMap typeMap exprMap  log) = 
    case Map.lookup varname typeMap of
      Just (t1:t2:t3) -> mytrace ("*** delete type: " ++ varname) ContextT adtMap constructorMap (Map.update (\xs -> Just $ init xs) varname typeMap) exprMap  log
      Just [t] -> mytrace ("*** delete type: " ++ varname) ContextT adtMap constructorMap (Map.delete varname typeMap) exprMap  log
      _ -> mytrace ("*** delete Nothing") context
  
  -- expr binding context operations
  containExpr :: ContextT -> String -> Bool
  containExpr context varname = Map.member varname (exprMap context)
  
  lookupExpr :: ContextT -> String -> Maybe Expr
  lookupExpr context varname = mytrace ("*** lookup expr: " ++ varname ++ " == " ++ (show e)) e 
        where e = case Map.lookup varname (exprMap context) of
                    Just ec -> Just $ head ec
                    _ -> Nothing
  
  insertExpr :: String -> Expr -> ContextT -> ContextT
  insertExpr varname expr context@(ContextT adtMap constructorMap typeMap exprMap  log) =  
    if containExpr context varname
    then mytrace ("*** insert expr: " ++ varname ++ " := " ++ show expr) ContextT adtMap constructorMap typeMap (Map.update (\x -> Just (expr : x)) varname exprMap)  log
    else mytrace ("*** insert expr: " ++ varname ++ " := " ++ show expr) ContextT adtMap constructorMap typeMap (Map.insert varname [expr] exprMap)  log
  
  deleteExpr :: String -> ContextT -> ContextT
  deleteExpr varname context@(ContextT adtMap constructorMap typeMap exprMap  log) = 
    case Map.lookup varname exprMap of
      Just (t1:t2:t3) -> mytrace ("*** delete expr: " ++ varname) ContextT adtMap constructorMap typeMap (Map.update (\x -> Just (tail x)) varname exprMap)  log
      Just [t] -> mytrace ("*** delete expr: " ++ varname) ContextT adtMap constructorMap typeMap (Map.delete varname exprMap)  log
      _ -> mytrace ("*** delete Nothing") context
  
      
  -- log operations
  prependLog :: String -> ContextT -> ContextT
  prependLog newLog (ContextT adtMap constructorMap typeMap exprMap  log) = ContextT adtMap constructorMap typeMap exprMap  (newLog:log)
  
  printLogs :: ContextT -> IO ()
  printLogs (ContextT adtMap constructorMap typeMap exprMap  log) = printStrLns log
  
  
  printStrLns :: [String] -> IO()
  printStrLns [] = putStrLn "end"
  printStrLns (s:ss') = do
                  putStrLn s
                  printStrLns ss'