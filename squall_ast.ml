(* SQUALL controlled language Lambda AST *)

let mk_var =
  let cpt = ref 0 in fun () -> incr cpt; "__x__" ^ (string_of_int !cpt)
;;

type lambda_term =
  | LVar of string
  | LApp of lambda_ast * lambda_ast
  | LLam of string * lambda_ast
  | LStat of lambda_ast * lambda_ast * lambda_ast
  | LNot of lambda_ast
  | LAnd of lambda_ast * lambda_ast
  | LOr of lambda_ast * lambda_ast
  | LOption of lambda_ast

and lambda_ast =
  lambda_term

[@@deriving show { with_path = false }]
;;

let unit_val = assert false;;
let true_fun = LLam("_", LTrue);;
