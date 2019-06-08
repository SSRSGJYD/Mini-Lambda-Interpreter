import AST
import EvalValue
import EvalType
import Control.Monad.State

makeFun :: (String, Type) -> [(String, Type)] -> Expr -> (Expr -> Expr)
makeFun (fn, rt) ((p, t):pts) body =
  let helper [] = body
      helper ((p0, t0):rs) = ELambda (p0, t0) (helper rs)
      ts = map snd pts ++ [rt]
  in ELetRec fn (p, t) (helper pts, foldr1 TArrow ts)

callFun :: Expr -> [Expr] -> Expr
callFun f [e] = EApply f e
callFun f (e:es) = callFun (EApply f e) es


tRaw_02_church0 =
  Program [] $

  ELet ("zero", ELambda ("f", TArrow TInt TInt)
                (ELambda ("x", TInt) (EVar "x"))) $
  ELet ("succ", ELambda ("n", TArrow (TArrow TInt TInt) (TArrow TInt TInt))
                (ELambda ("f", TArrow TInt TInt)
                  (ELambda ("x", TInt)
                    (EApply (EVar "f")
                      (callFun (EVar "n") [EVar "f", EVar "x"]))))) $
  -- plus = \a b f x -> a f (b f x)
  ELet ("plus", ELambda ("a", TArrow (TArrow TInt TInt) (TArrow TInt TInt))
                (ELambda ("b", TArrow (TArrow TInt TInt) (TArrow TInt TInt))
                  (ELambda ("f", TArrow TInt TInt)
                    (ELambda ("x", TInt)
                      (ELet ("af", EApply (EVar "a") (EVar "f"))
                        (ELet ("bf", EApply (EVar "b") (EVar "f"))
                          (EApply (EVar "af") (EApply (EVar "bf") (EVar "x"))))))))) $
  -- mult = \a b f -> b (a f)
  ELet ("mult", ELambda ("a", TArrow (TArrow TInt TInt) (TArrow TInt TInt))
                (ELambda ("b", TArrow (TArrow TInt TInt) (TArrow TInt TInt))
                  (ELambda ("f", TArrow TInt TInt)
                    (EApply (EVar "b") (EApply (EVar "a") (EVar "f")))))) $

  ELet ("f", ELambda ("x", TInt) (EAdd (EVar "x") (EIntLit 1))) $
  ELet ("one", EApply (EVar "succ") (EVar "zero")) $
  ELet ("two", EApply (EVar "succ") (EVar "one")) $
  ELet ("three", EApply (EVar "succ") (EVar "two")) $
  ELet ("five", callFun (EVar "plus") [EVar "two", EVar "three"]) $
  ELet ("six", callFun (EVar "mult") [EVar "two", EVar "three"]) $
  EAdd
  (callFun (EVar "six") [EVar "f", EIntLit 0])
  (callFun (EVar "five") [EVar "f", EIntLit 0])

  
main :: IO ()
main = do
  putStrLn " ---------- make `stack test` looks prettier ----------"
  
  -- print $ EvalValue.evalValue tRaw_01_lcm
  print $ EvalType.evalType tRaw_02_church0

