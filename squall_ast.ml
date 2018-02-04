(* SQUALL controlled language Lambda AST *)

let mk_var =
  let cpt = ref 0 in fun () -> incr cpt; "__x__" ^ (string_of_int !cpt)
;;

type lambda_term =
  |LVar of string
  |LApp of lambda_ast * lambda_ast
  |LLam of string * lambda_ast
  |LStat of lambda_ast * lambda_ast * lambda_ast
  |LNot of lambda_ast
  |LAnd of lambda_ast * lambda_ast
  |LOr of lambda_ast * lambda_ast
  |LOption of lambda_ast

and lambda_tag =
  |S_tag |NP_tag |VP_tag |P1_tag |P2_tag
  |Det_tag |NG1_tag |NG2_tag |AR_tag
  |App_tag |Rel_tag |AP_tag
  |Term_tag |None_tag

and lambda_ast =
  lambda_term * lambda_tag

[@@deriving show { with_path = false }]
;;

let mk_t t sem = (t, sem);;

let mk_tn t = (t, None_tag);;
