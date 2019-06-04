module GenCode where

import AST
import Control.Monad.State
import Data.Char
import System.IO
import qualified MiniParser

newtype Context = Context { funcId :: Int }

type ContextState a = State Context a

insertDecl :: Context -> Context
insertDecl (Context funcId) = Context (funcId+1)

gen :: Expr -> ContextState (String, String)
gen (EBoolLit x) = return (map toLower (show x), "")
gen (EIntLit x) = return (show x, "")
gen (ECharLit x) = return (show x, "")

gen (ENot e) = do
    c <- gen e
    return ("!(" ++ fst c ++ ")", snd c)

gen (EAnd e1 e2) = do
    c1 <- gen e1
    c2 <- gen e2
    return ("(" ++ fst c1 ++ ") && (" ++ fst c2 ++ ")", snd c1 ++ snd c2)

gen (EOr e1 e2) = do
    c1 <- gen e1
    c2 <- gen e2
    return ("(" ++ fst c1 ++ ") || (" ++ fst c2 ++ ")", snd c1 ++ snd c2)

gen (EAdd e1 e2) =  do
    c1 <- gen e1    
    c2 <- gen e2
    return ("(" ++ fst c1 ++ ") + (" ++ fst c2 ++ ")", snd c1 ++ snd c2)
gen (ESub e1 e2) =  do
    c1 <- gen e1
    c2 <- gen e2
    return ("(" ++ fst c1 ++ ") - (" ++ fst c2 ++ ")", snd c1 ++ snd c2)
gen (EMul e1 e2) =  do
    c1 <- gen e1
    c2 <- gen e2
    return ("(" ++ fst c1 ++ ") * (" ++ fst c2 ++ ")", snd c1 ++ snd c2)
gen (EDiv e1 e2) =  do
    c1 <- gen e1
    c2 <- gen e2
    return ("parseInt((" ++ fst c1 ++ ") / (" ++ fst c2 ++ "))", snd c1 ++ snd c2)
gen (EMod e1 e2) =  do
    c1 <- gen e1
    c2 <- gen e2
    return ("(" ++ fst c1 ++ ") % (" ++ fst c2 ++ ")", snd c1 ++ snd c2)

gen (EEq e1 e2) = do
    c1 <- gen e1
    c2 <- gen e2
    return ("(" ++ fst c1 ++ ") === (" ++ fst c2 ++ ")", snd c1 ++ snd c2)
gen (ENeq e1 e2) = do
    c1 <- gen e1
    c2 <- gen e2
    return ("(" ++ fst c1 ++ ") !== (" ++ fst c2 ++ ")", snd c1 ++ snd c2)

gen (ELt e1 e2) = do
    c1 <- gen e1
    c2 <- gen e2
    return ("(" ++ fst c1 ++ ") < (" ++ fst c2 ++ ")", snd c1 ++ snd c2)
gen (EGt e1 e2) = do
    c1 <- gen e1
    c2 <- gen e2
    return ("(" ++ fst c1 ++ ") > (" ++ fst c2 ++ ")", snd c1 ++ snd c2)
gen (ELe e1 e2) = do
    c1 <- gen e1
    c2 <- gen e2
    return ("(" ++ fst c1 ++ ") <= (" ++ fst c2 ++ ")", snd c1 ++ snd c2)
gen (EGe e1 e2) = do
    c1 <- gen e1
    c2 <- gen e2
    return ("(" ++ fst c1 ++ ") >= (" ++ fst c2 ++ ")", snd c1 ++ snd c2)

-- function _tmp(){
--     if-decl; 
--     if()
--     then {
--         then-decl; 
--         return ..;
--     } else{
--         else-decl; 
--         return ..;
--          }
--     }
gen (EIf e e1 e2) = do
    c <- gen e
    c1 <- gen e1
    c2 <- gen e2
    context <- get
    case context of
        Context funcId -> do
            modify insertDecl
            return ("_tmp" ++ show funcId ++ "()", "function _tmp" ++ show funcId ++ "(){\n"
                        ++ snd c ++ 
                        "if(" ++ fst c ++ "){\n    " 
                        ++ snd c1 ++ "return " ++ fst c1
                    ++ "\n} else{\n    "
                        ++ snd c2 ++ "return " ++ fst c2
                    ++ "\n}\n"
                    ++ "\n}\n")

-- function _tmp(varname) {
--     decl;
--     return ..;
-- }
gen (ELambda (varname, t1) e) = do
    c <- gen e
    context <- get
    case context of 
        Context funcId -> do
            modify insertDecl
            return ("_tmp" ++ show funcId, "function _tmp" ++ show funcId ++ "(" ++ tail (init (show varname)) ++"){\n"
                        ++ snd c ++ "return " ++ fst c ++ ";}\n")


-- function _tmp(x) {
--      decl;
-- 		return ..;
-- 	}
gen (ELet (varname, e1) e2) = do
    c1 <- gen e1
    c2 <- gen e2
    context <- get
    case context of
        Context funcId -> do
            modify insertDecl
            let decl = "function _tmp" ++ show funcId ++ "(" ++ tail (init (show varname)) ++ "){\n"
                        ++ snd c2 ++ 
                        "return " ++ fst c2 ++ ";}\n"
            return ("_tmp" ++ show funcId ++ "(" ++ fst c1 ++ ")", decl ++ snd c1)


gen (ELetRec funcname (argname,argtype) (funcExpr, returntype) expr) = do
    c1 <- gen funcExpr
    c2 <- gen expr
    return (fst c2, "function " ++ funcname ++ "(" ++ argname ++ "){\n" ++ snd c1 ++ 
                        "return " ++ fst c1 ++ "}\n" ++ snd c2)

gen (EApply e1 e2) = do
    c1 <- gen e1
    c2 <- gen e2
    return ("(" ++ fst c1 ++ ")" ++ "(" ++ fst c2 ++ ")", snd c1 ++ snd c2)

gen (EVar varname) = return (varname, [])

genCode :: Expr -> String
genCode expr = 
    case runState (gen expr) $ Context { funcId = 0 } of
        (a,s) -> snd a ++ "\nconsole.log(" ++ fst a ++ ")"

-- run (  ELet ("y", EIntLit 2) (ELet ("x", EIntLit 1) (EEq (EVar "y") (EVar "x")))) "output.js"
-- run (EApply (EApply (ELambda ("y", TInt) (ELambda ("x", TInt) (EAdd (EVar "y") (EVar "x")))) (EIntLit 1)) (EIntLit 2)) "output.js"
runGen :: Expr -> String -> IO ()
runGen expr path = do
    handle <- openFile path WriteMode
    hPutStr handle $ genCode expr
    hClose handle


main :: IO()
main = do
    input <- getLine
    case MiniParser.runMiniParser input of
        Left error -> print error
        Right a -> runGen a "output.js"