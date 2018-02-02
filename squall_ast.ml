(* SQUALL controlled language Lambda AST *)

let mk_var =
  let cpt = ref 0 in fun () -> incr cpt; "__x__" ^ (string_of_int !cpt)
;;

type lambda_term =
  |Var of string
  |App of lambda_ast * lambda_ast
  |Lam of string * lambda_term
  |Stat of lambda_ast * lambda_ast * lambda_ast
  |Not of lambda_ast
  |And of lambda_ast
  |Or of lambda_ast
  |Option of lambda_ast

and lambda_tag =
  |S |NP |VP |P1 |P2
  |Det |NG1 |NG2 |AR
  |App |Rel |AP
  |Term |None

and lambda_ast =
  lambda_term * lambda_tag
;;
