module EvalValue (evalValue, evalProgram) where

  import AST
  import ContextV
  import Control.Monad.State
  import qualified Data.Map as Map
  import Util
  import Pattern
  
  getBool :: Expr -> ContextStateV Bool
  getBool e = do
    ev <- EvalValue.eval e
    case ev of
      VBool b -> return b
      _ -> lift Nothing
  
  getInt :: Expr -> ContextStateV Int
  getInt e = do
    ev <- EvalValue.eval e
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
    ev1 <- EvalValue.eval e1
    ev2 <- EvalValue.eval e2
    return (VBool $ ev1 == ev2)
  
  eval (ENeq e1 e2) = do
    ev1 <- EvalValue.eval e1
    ev2 <- EvalValue.eval e2
    return (VBool $ ev1 /= ev2)
  
  eval (ELt e1 e2) = do
    ev1 <- EvalValue.eval e1
    ev2 <- EvalValue.eval e2
    return (VBool $ ev1 < ev2)
  
  eval (EGt e1 e2) = do
    ev1 <- EvalValue.eval e1
    ev2 <- EvalValue.eval e2
    return (VBool $ ev1 > ev2)
  
  eval (ELe e1 e2) = do
    ev1 <- EvalValue.eval e1
    ev2 <- EvalValue.eval e2
    return (VBool $ ev1 <= ev2)
  
  eval (EGe e1 e2) = do
    ev1 <- EvalValue.eval e1
    ev2 <- EvalValue.eval e2
    return (VBool $ ev1 >= ev2)
      
  eval (EIf e1 e2 e3) = do
    ev1 <- EvalValue.eval e1
    case ev1 of
      VBool True -> mytrace ("[if condition true] EvalValue.eval: " ++ show e2) EvalValue.eval e2
      VBool False -> mytrace ("[if condition false] EvalValue.eval: " ++ show e3) EvalValue.eval e3
  
  -- Lambda
  eval (ELambda (varname, vartype) e) = do
    context <- get
    if emptyArg context
    then mytrace "here" lift Nothing
    else
      let ec = firstArg context in do
        modify popArg
        modify (insertExpr varname ec)
        ev <- mytrace ("[ELambda] EvalValue.eval: " ++ show e) EvalValue.eval e
        modify (deleteExpr varname)
        return ev
  
  -- let
  eval (ELet (varname, e1) e2) = do
    context <- get
    modify (insertExpr varname (e1, context))
    t <- mytrace ("[ELet] EvalValue.eval: " ++ show e2) EvalValue.eval e2
    modify (deleteExpr varname)
    return t
  
  -- letrec
  eval (ELetRec funcname (argname,argtype) (funcExpr, returntype) expr) = do
    context <- get
    modify (insertRecExpr funcname (ELambda (argname, argtype) funcExpr, context))
    t <- mytrace ("[ELetRec] EvalValue.eval: " ++ show expr) EvalValue.eval expr
    modify (deleteRecExpr funcname)
    return t
  
  -- variable
  eval (EVar varname) = do
    context <- get
    case lookupExpr context varname of 
        Just ec -> do
            put $ snd ec
            -- modify (insertExpr varname ec)
            ev <- mytrace ("[EVar] EvalValue.eval: " ++ show (fst ec)) EvalValue.eval (fst ec)
            -- modify (deleteExpr varname)
            put context
            return ev
        Nothing -> 
          case lookupRecExpr context varname of 
            Just ec -> do
                put $ snd ec
                -- modify (insertExpr varname ec)
                ev <- mytrace ("[EVar] EvalValue.eval: " ++ show (fst ec)) EvalValue.eval (fst ec)
                -- modify (deleteExpr varname)
                put context
                return ev
            Nothing ->  -- ADT constructor without arguments
                case lookupConstructor context varname of
                    Just (adtname, []) -> 
                      mytrace ("[EVar] EvalValue.eval: " ++ show (EConstructor varname [])) EvalValue.eval (EConstructor varname [])
                    _ -> lift Nothing
    
  
  -- function apply
  eval (EApply e1 e2) = 
    case e1 of 
      ELambda e3@(varname, _) e4 -> do
        context <- get
        modify (pushArg (e2, context))
        mytrace ("[EApply] EvalValue.eval: " ++ show e1) EvalValue.eval e1
      EApply e3 e4 -> do
        context <- get
        modify (pushArg (e2, context))
        mytrace ("[EApply] EvalValue.eval: " ++ show e1) EvalValue.eval e1
      EVar funcname -> do
        context <- get
        case lookupExpr context funcname of
          Just ec -> 
            -- modify (deleteExpr funcname)
            -- modify (insertExpr funcname (fst ec, pushArg (e2, context) (snd ec)))
            -- put $ snd ec
            -- modify (pushArg (e2, context))
            -- put context
            mytrace ("[EApply] EvalValue.eval: " ++ show (EVar funcname)) EvalValue.evalVar (EVar funcname) (e2, context)
            -- put context
            -- return ev
          _ -> case lookupRecExpr context funcname of
              Just ec -> do
                put $ snd ec
                modify (insertExpr funcname ec)
                put context
                modify (pushArg (e2, context))
                ev <- mytrace ("[E] EvalValue.eval: " ++ show (fst ec)) EvalValue.eval (fst ec)
                put $ snd ec
                modify (deleteExpr funcname)
                put context
                return ev
              _ -> case lookupConstructor context funcname of
                      Just (adtname, argList) -> do
                        modify (pushArg (e2, context))
                        mytrace ("[EApply] EvalValue.eval: " ++ show (EConstructor funcname [])) EvalValue.eval (EConstructor funcname [])
                      _ -> lift Nothing
      _ -> lift Nothing
  
  -- case
  eval (ECase e list) = do
    ev <- EvalValue.eval e
    case list of
      (p : ps) -> do 
        ebool <- matchPattern (fst p) ev
        if ebool
        then do
          modify (bindPattern (fst p) ev)
          result <- mytrace ("[ECase] EvalValue.eval: " ++ show (snd p)) EvalValue.eval (snd p)
          modify (unbindPattern (fst p))
          return result
        else
          mytrace ("[ECase] EvalValue.eval: " ++ show (ECase e ps)) EvalValue.eval (ECase e ps)
      _ -> lift Nothing
  
  -- ADT constructor, similar to multi-lambda expression
  eval (EConstructor constructor argList) = do
    context <- get
    case lookupConstructor context constructor of
      Just (adtname, typeList)
        | length argList == length typeList ->
          do
            evs <- mytrace ("[EConstructor] evalExprList: " ++ show argList) evalExprList argList
            return $ VData adtname constructor evs
        | otherwise -> 
          if emptyArg context
            then lift Nothing
            else
              let ec = firstArg context in do  
                modify popArg
                -- TODO
                ev <- EvalValue.eval $ fst ec
                let e' = wrapValueToExpr ev
                mytrace ("[EConstructor] EvalValue.eval" ++ show (EConstructor constructor (argList ++ [e']))) EvalValue.eval $ EConstructor constructor (argList ++ [e'])
                
  -- variable with arg
  evalVar :: Expr -> (Expr, ContextV) -> ContextStateV Value
  evalVar (EVar varname) arg = do
    context <- get
    case lookupExpr context varname of 
        Just ec -> do
            -- put $ snd ec
            modify (pushArg arg)
            -- modify (insertExpr varname ec)
            ev <- mytrace ("[EVar] EvalValue.evalVar: " ++ show (fst ec)) EvalValue.eval (fst ec)
            -- modify (deleteExpr varname)
            -- put context
            return ev
        Nothing -> 
          case lookupRecExpr context varname of 
            Just ec -> do
                put $ snd ec
                -- modify (insertExpr varname ec)
                ev <- mytrace ("[EVar] EvalValue.eval: " ++ show (fst ec)) EvalValue.eval (fst ec)
                -- modify (deleteExpr varname)
                put context
                return ev
            Nothing ->  -- ADT constructor without arguments
                case lookupConstructor context varname of
                    Just (adtname, []) -> 
                      mytrace ("[EVar] EvalValue.eval: " ++ show (EConstructor varname [])) EvalValue.eval (EConstructor varname [])
                    _ -> lift Nothing


  evalExprList :: [Expr] -> ContextStateV [Value]
  evalExprList [] = return []
  evalExprList (e:es) = do
    ev <- EvalValue.eval e
    esv <- evalExprList es
    return (ev:esv)
  

  evalValue :: Program -> Result
  evalValue p = case evalProgram p of
    Just v -> parseValueToResult v
    Nothing -> RInvalid
  
  evalProgram :: Program -> Maybe Value
  evalProgram (Program adts body) = evalStateT (EvalValue.eval body) $
    ContextV { adtMap = initAdtMap adts, 
              constructorMap = initConstructorMap adts,
              typeMap = Map.empty, 
              exprMap = Map.empty,
              exprRecMap = Map.empty,
              argList = [],
              logList = ["start EvalValue Program"] }
  
