module Pattern where

import AST
import ContextV
import Util

matchPatterns :: [Pattern] -> [Value] -> ContextStateV Bool
matchPatterns [] [] = return True
matchPatterns _ [] = return False
matchPatterns [] _ = return False
matchPatterns (p:ps') (ev:evs') = do
  ebool <- matchPattern p ev
  if ebool
  then matchPatterns ps' evs' 
  else return False
    
matchPattern :: Pattern -> Value -> ContextStateV Bool
matchPattern p ev =
  case p of
    PBoolLit x -> return $ ev == VBool x
    PIntLit x -> return $ ev == VInt x
    PCharLit x -> return $ ev == VChar x
    PVar varname -> return True
    PData funcname patterns -> case ev of
      VData adtname constructor valueList -> 
        if funcname == constructor
        then matchPatterns patterns valueList
        else return False
      _ -> return False
    _ -> return False 

bindPatterns :: [Pattern] -> [Value] -> ContextV -> ContextV
bindPatterns (p:ps) (v:vs) context = let context' = bindPattern p v context
                                     in bindPatterns ps vs context'
bindPatterns _ _ context = context


bindPattern :: Pattern -> Value -> ContextV -> ContextV
bindPattern p ev context = 
  case p of
    PBoolLit x -> context
    PIntLit x -> context
    PCharLit x -> context
    PVar varname -> insertExpr varname (wrapValueToExpr ev, context) context
    PData funcname [] -> context
    PData constructor patterns -> case ev of
      VData adtname constructor argList -> bindPatterns patterns argList context
      _ -> context
    _ -> context


unbindPatterns :: [Pattern] -> ContextV -> ContextV
unbindPatterns (p:ps) context = let context' = unbindPattern p context
                                in unbindPatterns ps context'
unbindPatterns _ context = context


unbindPattern :: Pattern -> ContextV -> ContextV
unbindPattern p context = 
  case p of
    PBoolLit x -> context
    PIntLit x -> context
    PCharLit x -> context
    PVar varname -> deleteExpr varname context
    PData constructor patterns -> unbindPatterns patterns context
    _ -> context