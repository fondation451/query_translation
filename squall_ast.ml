(* SQUALL controlled language Lambda AST *)

let mk_var =
  let cpt = ref 0 in fun () -> incr cpt; "?__x__" ^ (string_of_int !cpt)

type lambda_term =
  | LVar of string
  | LInt of int
  | LApp of lambda_ast * lambda_ast
  | LLam of string * lambda_ast
  | LStat of lambda_ast * lambda_ast * lambda_ast
  | LNot of lambda_ast
  | LAnd of lambda_ast * lambda_ast
  | LOr of lambda_ast * lambda_ast
  | LOption of lambda_ast
  | LInit of lambda_ast
  | LExists of lambda_ast
  | LThe of lambda_ast * lambda_ast
  | LTrue
  | LBind of lambda_ast * lambda_ast
  | LWhere of lambda_ast * lambda_ast
  | LEq of lambda_ast * lambda_ast
  | LAsk of lambda_ast
  | LSelect of lambda_ast
  | LCount of lambda_ast
  | LForall of lambda_ast * lambda_ast
  | LAtleast of int * lambda_ast

  (* Mes Rajouts *)
  |LTriple of lambda_ast * lambda_ast * lambda_ast
  |LPred2 of pred * lambda_ast * lambda_ast
  |LPred1 of pred * lambda_ast
  |LAggreg of string * lambda_ast * lambda_ast * lambda_ast list

and lambda_ast =
  lambda_term

and  pred =
  |Pred_EQ
  |Pred_LT
  |Pred_LE
  |Pred_GT
  |Pred_GE

[@@deriving show { with_path = false }]

let thing = LLam("thing",
  LStat(LVar "thing", LVar "rdf:type", LVar "rdfs:Resource"))
let true_fun = LLam("_", LTrue)
