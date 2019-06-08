module Pattern where

import AST
import Control.Monad.State
import ContextV
import ContextT
import Util

-- matchPatterns :: [Pattern] -> [Value] -> ContextStateV Bool
-- matchPatterns [] [] = return True
-- matchPatterns _ [] = return False
-- matchPatterns [] _ = return False
-- matchPatterns (p:ps') (ev:evs') = do
--   ebool <- matchPattern p ev
--   if ebool
--   then matchPatterns ps' evs' 
--   else return False
    
-- matchPattern :: Pattern -> Value -> ContextStateV Bool
-- matchPattern p ev =
--   case p of
--     PBoolLit x -> return $ ev == VBool x
--     PIntLit x -> return $ ev == VInt x
--     PCharLit x -> return $ ev == VChar x
--     PVar varname -> return True
--     PData funcname patterns -> case ev of
--       VData adtname constructor valueList -> 
--         if funcname == constructor
--         then matchPatterns patterns valueList
--         else return False
--       _ -> return False
--     _ -> return False 

-- bindPatterns :: [Pattern] -> [Value] -> ContextV -> ContextV
-- bindPatterns (p:ps) (v:vs) context = let context' = bindPattern p v context
--                                      in bindPatterns ps vs context'
-- bindPatterns _ _ context = context


-- bindPattern :: Pattern -> Value -> ContextV -> ContextV
-- bindPattern p ev context = 
--   case p of
--     PBoolLit x -> context
--     PIntLit x -> context
--     PCharLit x -> context
--     PVar varname -> ContextV.insertExpr varname (wrapValueToExpr ev, context) context
--     PData funcname [] -> context
--     PData constructor patterns -> case ev of
--       VData adtname constructor argList -> bindPatterns patterns argList context
--       _ -> context
--     _ -> context


-- unbindPatterns :: [Pattern] -> ContextV -> ContextV
-- unbindPatterns (p:ps) context = let context' = unbindPattern p context
--                                 in unbindPatterns ps context'
-- unbindPatterns _ context = context


-- unbindPattern :: Pattern -> ContextV -> ContextV
-- unbindPattern p context = 
--   case p of
--     PBoolLit x -> context
--     PIntLit x -> context
--     PCharLit x -> context
--     PVar varname -> ContextV.deleteExpr varname context
--     PData constructor patterns -> unbindPatterns patterns context
--     _ -> context

-- match pattern for type
matchPatternsT :: [Pattern] -> [Type] -> ContextStateT Bool
matchPatternsT [] [] = return True
matchPatternsT _ [] = return False
matchPatternsT [] _ = return False
matchPatternsT (p:ps') (ev:evs') = do
  ebool <- matchPatternT p ev
  if ebool
  then matchPatternsT ps' evs' 
  else return False
    
matchPatternT :: Pattern -> Type -> ContextStateT Bool
matchPatternT p ev = 
  case p of
    PBoolLit x -> return $ ev == TBool
    PIntLit x -> return $ ev == TInt
    PCharLit x -> return $ ev == TChar
    PVar varname -> return True
    PData funcname patterns -> case ev of
      TConstructor adtname constructor typeList -> 
        if funcname == constructor
        then matchPatternsT patterns typeList
        else return False
      TData adtname -> do
        context <- get
        case ContextT.lookupConstructor context funcname of
          Just (adtname', _) -> return $ adtname == adtname'
          _ -> return False
      _ -> return False
    _ -> return False 

bindPatternsT :: [Pattern] -> [Type] -> ContextT -> ContextT
bindPatternsT (p:ps) (v:vs) context = let context' = bindPatternT p v context
                                     in bindPatternsT ps vs context'
bindPatternsT _ _ context = context


bindPatternT :: Pattern -> Type -> ContextT -> ContextT
bindPatternT p ev context = 
  case p of
    PBoolLit x -> context
    PIntLit x -> context
    PCharLit x -> context
    PVar varname -> ContextT.insertType varname ev context
    PData funcname [] -> context
    PData constructor patterns -> case ev of
      TConstructor adtname constructor argList -> bindPatternsT patterns argList context
      TData adtname -> 
        case ContextT.lookupConstructor context constructor of
          Just (adtname, typeList) -> bindPatternsT patterns typeList context
          _ -> context
      _ -> context
    _ -> context


unbindPatternsT :: [Pattern] -> ContextT -> ContextT
unbindPatternsT (p:ps) context = let context' = unbindPatternT p context
                                in unbindPatternsT ps context'
unbindPatternsT _ context = context


unbindPatternT :: Pattern -> ContextT -> ContextT
unbindPatternT p context = 
  case p of
    PBoolLit x -> context
    PIntLit x -> context
    PCharLit x -> context
    PVar varname -> ContextT.deleteExpr varname context
    PData constructor patterns -> unbindPatternsT patterns context
    _ -> context