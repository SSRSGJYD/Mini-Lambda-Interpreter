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

test_sum3 =
  Program [] $

  makeFun ("sum3", TInt) [("x", TInt), ("y", TInt), ("z", TInt)] (
  EAdd (EAdd (EVar "x") (EVar "y")) (EVar "z")
  ) $

  callFun (EVar "sum3") [EIntLit 1, EIntLit 2, EIntLit 3]

test_fbi =
  Program [] $

  makeFun ("fbi", TInt) [("x", TInt)] (
  EIf (ELe (EVar "x") (EIntLit 1))
    (EIntLit 1)
    (EAdd
      (EApply (EVar "fbi") (ESub (EVar "x") (EIntLit 1)))
      (EApply (EVar "fbi") (ESub (EVar "x") (EIntLit 2))))
  ) $

  callFun (EVar "fbi") [EIntLit 10]

test_adt_ctor =
  Program
  [ ADT "tuple3" [("tuple3", [TInt, TInt, TInt])]
  ] $

  makeFun ("sum3", TInt) [("x", TInt), ("y", TInt), ("z", TInt)] (
  EAdd (EAdd (EVar "x") (EVar "y")) (EVar "z")
  ) $
  ELet ("v", callFun (EVar "tuple3") [EIntLit 1, EIntLit 2, EIntLit 3]) $
  ECase (EVar "v")
  [ (PData "tuple3" [PVar "x", PVar "y", PVar "z"],
     callFun (EVar "sum3") [EVar "x", EVar "y", EVar "z"])
  , (PVar "_",
     EIntLit 2333333)
  ]

test_adt_case =
  Program
  [ ADT "tuple3" [("tuple3", [TInt, TInt, TInt])]
  ] $

  ELet ("x", callFun (EVar "tuple3") [EIntLit 1, EIntLit 2, EIntLit 3]) $
  ECase (EVar "x")
  [ (PData "tuple3" [PIntLit 1, PIntLit 3, PVar "y"],
     ECharLit '1')
  , (PData "tuple3" [PIntLit 1, PIntLit 2, PVar "y"],
     ECharLit '2')
  ]

test_adt_list =
  Program
  [ ADT "List" [ ("Cons", [TInt, TData "List"])
               , ("Nil", [])
               ]
  ] $

  ELet ("x", EVar "Nil") $
  ELet ("y", callFun (EVar "Cons") [EIntLit 2, EVar "x"]) $
  ELet ("z", callFun (EVar "Cons") [EIntLit 3, EVar "y"]) $
  EVar "z"

test_adt_list_range =
  Program
  [ ADT "List" [ ("Cons", [TInt, TData "List"])
               , ("Nil", [])
               ]
  ] $

  makeFun ("range", TData "List") [("n", TInt)] (
  EIf (ELe (EVar "n") (EIntLit 0))
    (EVar "Nil")
    (callFun (EVar "Cons") [EVar "n", callFun (EVar "range") [ESub (EVar "n") (EIntLit 1)]])
  ) $
  
  callFun (EVar "range") [EIntLit 10]

test_adt_list_sum =
  Program
  [ ADT "List" [ ("Cons", [TInt, TData "List"])
               , ("Nil", [])
               ]
  ] $

  makeFun ("range", TData "List") [("n", TInt)]
  (
    EIf (ELe (EVar "n") (EIntLit 0))
    (EVar "Nil")
    (callFun (EVar "Cons") [EVar "n", callFun (EVar "range") [ESub (EVar "n") (EIntLit 1)]])
  ) $
  makeFun ("sum", TInt) [("l", TData "List")]
  (
    ECase (EVar "l")
    [ (PData "Cons" [PVar "x", PVar "xs"],
       EAdd (EVar "x") (callFun (EVar "sum") [EVar "xs"]))
    , (PData "Nil" [],
       EIntLit 0)
    ]
  ) $

  ELet ("l", callFun (EVar "range") [EIntLit 100]) $
  callFun (EVar "sum") [EVar "l"]

test_type =
  Program [] $
  ECase (EBoolLit True)
  [ (PBoolLit True, ECharLit '2')
  , (PBoolLit False, EIntLit 1)
  ]


test_bool_value =
  Program [] $
  ENot (EBoolLit False)

test_bool_type_1 =
  Program [] $
  ENot (EBoolLit False)
 
test_bool_type_2 =  
  Program [] $
  ENot (EIntLit 42)

test_int_add_value = 
  Program [] $
  EAdd (EIntLit 1) (EIntLit 2)

test_int_sub_value = 
  Program [] $
  ESub (EIntLit 2) (EIntLit 1)

test_int_mul_value = 
  Program [] $
  EMul (EIntLit 2) (EIntLit 3)

test_int_div_value = 
  Program [] $
  EDiv (EIntLit 5) (EIntLit 2)

test_bool_eq_value = 
  Program [] $
  EEq (EBoolLit True) (EBoolLit True)

test_int_eq_value = 
  Program [] $
  EEq (EIntLit 12) (EIntLit 12)

test_char_eq_value = 
  Program [] $
  EEq (ECharLit 'A') (ECharLit 'A')

test_bool_neq_value = 
  Program [] $
  ENeq (EBoolLit True) (EBoolLit True)

test_int_lt_value = 
  Program [] $
  ELt (EIntLit 12) (EIntLit 10)

test_char_lt_value = 
  Program [] $
  ELt (ECharLit 'B') (ECharLit 'A')

test_int_gt_value = 
  Program [] $
  EGt (EIntLit 12) (EIntLit 12)

test_char_gt_value = 
  Program [] $
  EGt (ECharLit 'A') (ECharLit 'A')

test_int_le_value = 
  Program [] $
  ELe (EIntLit 12) (EIntLit 12)

test_char_le_value = 
  Program [] $
  ELe (ECharLit 'A') (ECharLit 'A')

test_int_ge_value = 
  Program [] $
  EGe (EIntLit 12) (EIntLit 12)

test_char_ge_value = 
  Program [] $
  EGe (ECharLit 'A') (ECharLit 'A')

main :: IO ()
main = do
  putStrLn " ---------- make `stack test` looks prettier ----------"
  
  -- print $ EvalValue.evalValue test_bool_value -- should be: RBool True
  -- print $ EvalType.evalType test_bool_type_1 -- should be: Just TBool
  -- print $ EvalType.evalType test_bool_type_2 -- should be: Nothing
  
  -- print $ EvalValue.evalValue test_int_add_value
  -- print $ EvalValue.evalValue test_int_sub_value
  -- print $ EvalValue.evalValue test_int_mul_value
  -- print $ EvalValue.evalValue test_int_div_value

  -- print $ EvalValue.evalValue test_bool_eq_value
  -- print $ EvalValue.evalValue test_int_eq_value
  -- print $ EvalValue.evalValue test_char_eq_value

  -- print $ EvalValue.evalValue test_bool_neq_value
  -- print $ EvalValue.evalValue test_int_neq_value
  -- print $ EvalValue.evalValue test_char_neq_value

  print $ EvalValue.evalValue test_int_lt_value
  print $ EvalValue.evalValue test_char_lt_value
  print $ EvalValue.evalValue test_int_gt_value
  print $ EvalValue.evalValue test_char_gt_value
  print $ EvalValue.evalValue test_int_le_value
  print $ EvalValue.evalValue test_char_le_value
  print $ EvalValue.evalValue test_int_ge_value
  print $ EvalValue.evalValue test_char_ge_value



  -- print $ EvalValue.evalValue test_fbi
  -- print $ EvalValue.evalValue test_sum3
  -- print $ EvalValue.evalValue test_adt_ctor
  -- print $ EvalValue.evalValue test_adt_case
  -- print $ EvalValue.evalProgram test_adt_list
  -- print $ EvalValue.evalProgram test_adt_list_range
  -- print $ EvalValue.evalValue test_adt_list_sum
  -- print $ EvalType.evalType test_fbi
  -- print $ EvalType.evalType test_sum3
  -- print $ EvalType.evalType test_adt_ctor
  -- print $ EvalType.evalType test_adt_case
  -- print $ EvalType.evalType test_adt_list
  -- print $ EvalType.evalType test_adt_list_range
  -- print $ EvalType.evalType test_adt_list_sum
  -- print $ EvalType.evalType test_type
