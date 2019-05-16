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

-- add, sub, mul and div 
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

-- eq and neq
test_eq_different_type = 
  Program [] $
  EEq (EBoolLit True) (EIntLit 1)

test_bool_eq_value = 
  Program [] $
  EEq (EBoolLit True) (EBoolLit True)

test_int_eq_value = 
  Program [] $
  EEq (EIntLit 12) (EIntLit 12)

test_char_eq_value = 
  Program [] $
  EEq (ECharLit 'A') (ECharLit 'A')

test_neq_different_type = 
  Program [] $
  EEq (EBoolLit True) (EIntLit 1)

test_bool_neq_value = 
  Program [] $
  ENeq (EBoolLit True) (EBoolLit True)

test_int_neq_value = 
  Program [] $
  EEq (EIntLit 12) (EIntLit 13)

test_char_neq_value = 
  Program [] $
  EEq (ECharLit 'A') (ECharLit 'B')

-- compare: gt, lt, ge and le
test_lt_different_type = 
  Program [] $
  ELt (EIntLit 12) (ECharLit 'A')

test_lt_wrong_type = 
  Program [] $
  ELt (EBoolLit True) (EBoolLit True)

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

-- if
test_if_wrong_e1_type = 
  Program [] $
  EIf (EIntLit 1) (EIntLit 1) (EIntLit 2)

test_if_different_type = 
  Program [] $
  EIf (EGt (EIntLit 1) (EIntLit 2)) (EIntLit 1) (EBoolLit True)

test_if_value = 
  Program [] $
  EIf (EGt (EIntLit 1) (EIntLit 2)) (EIntLit 1) (EIntLit 2)

-- lambda expression
test_lambda_type = 
  Program [] $
  ELambda ("x", TInt) (EAdd (EIntLit 1) (EVar "x"))

-- let
test_let = 
  Program [] $
  ELet ("x", EIntLit 1) (EAdd (EIntLit 1) (EVar "x"))

test_nested_let = 
  Program [] $
  ELet ("y", EIntLit 2) (ELet ("x", EIntLit 1) (EEq (EVar "y") (EVar "x")))

-- apply
test_apply_lambda = 
  Program [] $
  EApply (ELambda ("x", TInt) (EAdd (EIntLit 1) (EVar "x"))) (EIntLit 2)

test_nested_lambda = 
  Program [] $
  EApply (EApply (ELambda ("y", TInt) (ELambda ("x", TInt) (EAdd (EVar "y") (EVar "x")))) (EIntLit 1)) (EIntLit 2)

-- letrec
test_letrec =
  Program [] $
  ELetRec "func" ("x", TInt) (EAdd (EIntLit 1) (EVar "x"), TInt) (EApply (EVar "func") (EIntLit 2))

test_letrec_recursive = 
  Program [] $
  ELetRec "func" ("x", TInt) (EAdd (EIntLit 1) (EVar "x"), TInt) (EApply (EVar "func") (EApply (EVar "func") (EIntLit 2)))

test_letrec_multi_args = 
  Program [] $
  ELetRec "sum" ("x",TInt) (ELambda ("y",TInt) (EAdd (EVar "x") (EVar "y")),TArrow TInt TInt) (EApply (EApply (EVar "sum") (EIntLit 1)) (EIntLit 2))

  -- case
test_case_bool = 
  Program [] $
  ECase (EGt (EIntLit 2) (EIntLit 1)) [(PBoolLit True, EIntLit 2), (PBoolLit False, EIntLit 1)]

test_case_int = 
  Program [] $
  ECase (EAdd (EIntLit 2) (EIntLit 1)) [(PBoolLit True, EIntLit 1), (PBoolLit False, EIntLit 1), (PIntLit 3, EIntLit 2)]

test_case_char = 
  Program [] $
  ECase (ECharLit 'A') [(PBoolLit True, EIntLit 1), (PBoolLit False, EIntLit 1), (PIntLit 3, EIntLit 1), (PCharLit 'A', EIntLit 2)]  

test_case_var = 
  Program [] $
  ECase (EIntLit 1) [(PBoolLit True, EIntLit 1), (PVar "x", EAdd (EVar "x") (EIntLit 1))]



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

  -- print $ EvalType.evalType test_eq_different_type
  -- print $ EvalValue.evalValue test_eq_different_type
  -- print $ EvalType.evalType test_neq_different_type
  -- print $ EvalValue.evalValue test_neq_different_type

  -- print $ EvalValue.evalValue test_bool_eq_value
  -- print $ EvalValue.evalValue test_int_eq_value
  -- print $ EvalValue.evalValue test_char_eq_value

  -- print $ EvalValue.evalValue test_bool_neq_value
  -- print $ EvalValue.evalValue test_int_neq_value
  -- print $ EvalValue.evalValue test_char_neq_value

  -- print $ EvalType.evalType test_lt_different_type
  -- print $ EvalValue.evalValue test_lt_different_type
  -- print $ EvalType.evalType test_lt_wrong_type
  -- print $ EvalValue.evalValue test_lt_wrong_type

  -- print $ EvalValue.evalValue test_int_lt_value
  -- print $ EvalValue.evalValue test_char_lt_value
  -- print $ EvalValue.evalValue test_int_gt_value
  -- print $ EvalValue.evalValue test_char_gt_value
  -- print $ EvalValue.evalValue test_int_le_value
  -- print $ EvalValue.evalValue test_char_le_value
  -- print $ EvalValue.evalValue test_int_ge_value
  -- print $ EvalValue.evalValue test_char_ge_value

  -- print $ EvalType.evalType test_if_wrong_e1_type 
  -- print $ EvalValue.evalValue test_if_wrong_e1_type 
  -- print $ EvalType.evalType test_if_different_type 
  -- print $ EvalValue.evalValue test_if_different_type 
  -- print $ EvalValue.evalValue test_if_value

  -- print $ EvalType.evalType test_lambda_type

  -- print $ EvalType.evalType test_let
  -- print $ EvalValue.evalValue test_let
  -- print $ EvalType.evalType test_nested_let
  -- print $ EvalValue.evalValue test_nested_let

  -- print $ EvalType.evalType test_apply_lambda
  -- print $ EvalValue.evalValue test_apply_lambda
  -- print $ EvalType.evalType test_nested_lambda
  -- print $ EvalValue.evalValue test_nested_lambda

  -- print $ EvalType.evalType test_letrec
  -- print $ EvalValue.evalValue test_letrec
  -- print $ EvalType.evalType test_letrec_recursive
  -- print $ EvalValue.evalValue test_letrec_recursive
  -- print $ EvalValue.evalValue test_letrec_multi_args
  
  -- print $ EvalType.evalType test_case_bool
  -- print $ EvalValue.evalValue test_case_bool
  -- print $ EvalType.evalType test_case_int
  -- print $ EvalValue.evalValue test_case_int
  -- print $ EvalType.evalType test_case_char
  -- print $ EvalValue.evalValue test_case_char
  -- print $ EvalType.evalType test_case_var
  -- print $ EvalValue.evalValue test_case_var

  print $ EvalValue.evalValue test_fbi
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
