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


tRaw_08_apply =
  Program [] $

  makeFun ("sum3", TInt) [("x1", TInt), ("x2", TInt), ("x3", TInt)]
  (
    EAdd (EAdd (EVar "x1") (EVar "x2")) (EVar "x3")
  ) $

  callFun (EVar "sum3") [EIntLit 1, EIntLit 1, EIntLit 1, EIntLit 1]

main :: IO ()
main = do
  putStrLn " ---------- make `stack test` looks prettier ----------"
  
  -- print $ EvalValue.evalValue tRaw_01_lcm
  print $ EvalType.evalType tRaw_08_apply

