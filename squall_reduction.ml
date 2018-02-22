(* SQUALL Lambda reduction *)

open Squall_ast;;

exception App_No_Lambda

let rec reduction s =
  let t, tag = s in
  match t with
  |LApp(s1, s2) -> begin
    match s1 with
    |LLam(x, s_lam), _ -> reduction (substitute s_lam x s2)
    |_ ->
      let new_s = (mk_t (LApp(reduction s1, reduction s2)) tag) in
      if s <> new_s then
        reduction new_s
      else
        new_s
  end
  |_ -> s

and substitute s x v =
  let t, tag = s in
  match t with
  |LVar(str) -> if str = x then v else s
  |LApp(s1, s2) -> mk_t (LApp(substitute s1 x v, substitute s2 x v)) tag
  |LLam(y, s1) -> mk_t (LLam(y, substitute s1 x v)) tag
  |LStat(s1, s2, s3) -> mk_t (LStat(substitute s1 x v, substitute s2 x v, substitute s3 x v)) tag
  |LNot(s1) -> mk_t (LNot(substitute s1 x v)) tag
  |LAnd(s1, s2) -> mk_t (LAnd(substitute s1 x v, substitute s2 x v)) tag
  |LOr(s1, s2) -> mk_t (LOr(substitute s1 x v, substitute s2 x v)) tag
  |LOption(s1) -> mk_t (LOption(substitute s1 x v)) tag
;;
