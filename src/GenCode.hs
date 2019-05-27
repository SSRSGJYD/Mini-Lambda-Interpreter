module GenCode where

import AST
import Control.Monad.State
import Data.Char
import System.IO

data Context = Context {  
                    funcId :: Int, 
                    funcDecl :: [String]
                    }

type ContextState a = State Context a

insertDecl :: String -> Context ->  Context
insertDecl decl c@(Context funcId funcDecl) = 
    Context (funcId+1) (decl : funcDecl)

gen :: Expr -> ContextState String
-- gen = undefined
gen (EBoolLit x) = return $ map toLower (show x)
gen (EIntLit x) = return $ show x
gen (ECharLit x) = return $ show x

gen (ENot e) = do
    c <- gen e
    return $ "!(" ++ c ++ ")"

gen (EAnd e1 e2) = do
    c1 <- gen e1
    c2 <- gen e2
    return $ "(" ++ c1 ++ ") && (" ++ c2 ++ ")"

gen (EOr e1 e2) = do
    c1 <- gen e1
    c2 <- gen e2
    return $ "(" ++ c1 ++ ") || (" ++ c2 ++ ")"

gen (EAdd e1 e2) =  do
    c1 <- gen e1
    c2 <- gen e2
    return $ "(" ++ c1 ++ ") + (" ++ c2 ++ ")"
gen (ESub e1 e2) =  do
    c1 <- gen e1
    c2 <- gen e2
    return $ "(" ++ c1 ++ ") - (" ++ c2 ++ ")"
gen (EMul e1 e2) =  do
    c1 <- gen e1
    c2 <- gen e2
    return $ "(" ++ c1 ++ ") * (" ++ c2 ++ ")"
gen (EDiv e1 e2) =  do
    c1 <- gen e1
    c2 <- gen e2
    return $ "parseInt((" ++ c1 ++ ") / (" ++ c2 ++ "))"
gen (EMod e1 e2) =  do
    c1 <- gen e1
    c2 <- gen e2
    return $ "(" ++ c1 ++ ") % (" ++ c2 ++ ")"

gen (EEq e1 e2) = do
    c1 <- gen e1
    c2 <- gen e2
    return $ "(" ++ c1 ++ ") === (" ++ c2 ++ ")"
gen (ENeq e1 e2) = do
    c1 <- gen e1
    c2 <- gen e2
    return $ "(" ++ c1 ++ ") !== (" ++ c2 ++ ")"

gen (ELt e1 e2) = do
    c1 <- gen e1
    c2 <- gen e2
    return $ "(" ++ c1 ++ ") < (" ++ c2 ++ ")"
gen (EGt e1 e2) = do
    c1 <- gen e1
    c2 <- gen e2
    return $ "(" ++ c1 ++ ") > (" ++ c2 ++ ")"
gen (ELe e1 e2) = do
    c1 <- gen e1
    c2 <- gen e2
    return $ "(" ++ c1 ++ ") <= (" ++ c2 ++ ")"
gen (EGe e1 e2) = do
    c1 <- gen e1
    c2 <- gen e2
    return $ "(" ++ c1 ++ ") >= (" ++ c2 ++ ")"

gen (EIf e e1 e2) = do
    c <- gen e
    c1 <- gen e1
    c2 <- gen e2
    return $ "if(" ++ c ++ "){\n    " 
                ++ c1
            ++ "\n} else{\n    "
                ++ c2
            ++ "\n}"

gen (ELambda (varname, t1) e) = do
    c <- gen e
    return $ "(" ++ tail (init (show varname)) ++ ")=>(" ++ c ++ ")"


-- function _tmp1() {
-- 		let x=1; 
-- 		return x+1;
-- 	}
gen (ELet (varname, e1) e2) = do
    c1 <- gen e1
    c2 <- gen e2
    context <- get
    case context of
        Context funcId funcDecl -> do
            let decl = "function _tmp" ++ show funcId ++ "(){let "
                        ++ tail (init (show varname)) ++ "=(" ++ c1 ++ "); return " 
                        ++ c2 ++ ";}\n" 
            modify $ insertDecl decl
            return $ "_tmp" ++ show funcId ++ "()"


-- gen (ELetRec funcname (argname,argtype) (funcExpr, returntype) expr) = do

gen (EApply e1 e2) = do
    c1 <- gen e1
    c2 <- gen e2
    return $ "(" ++ c1 ++ ")" ++ "(" ++ c2 ++ ")"

gen (EVar varname) = return varname

genCode :: Expr -> String
genCode expr = 
    case runState (gen expr) $ Context {
                                funcId = 0,
                                funcDecl = []
                            } of
        (a,s) -> 
            let decls = foldl (++) [] (funcDecl s)
            in decls ++ "\nconsole.log(" ++ a ++ ")"
            -- foldl (++) a (funcDecl s)

-- run (EApply (ELambda ("x", TInt) (EAdd (EIntLit 1) (EVar "x"))) (EIntLit 2)) "output.js"
run :: Expr -> String -> IO ()
run expr path = do
    handle <- openFile path WriteMode
    hPutStr handle $ genCode expr
    hClose handle