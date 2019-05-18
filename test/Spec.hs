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


-- | It can terminate under short-circuit evaluation.
-- It doesn't terminate under strict evaluation.
tRaw_06_and =
  Program [] $

  makeFun ("test", TBool) [("x", TBool)]
  (
    EOr (EVar "x") (callFun (EVar "test") [EBoolLit False])
  ) $

  callFun (EVar "test") [EBoolLit True]

main :: IO ()
main = do
  putStrLn " ---------- make `stack test` looks prettier ----------"
  
  -- print $ EvalValue.evalValue tRaw_01_lcm
  print $ EvalType.evalType tRaw_06_and

