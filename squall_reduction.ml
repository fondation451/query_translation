(* SQUALL Lambda reduction *)

open Squall_ast

exception App_No_Lambda

let rec reduction s = match s with
  | LApp(s1, s2) -> begin
    match s1 with
    | LLam(x, s_lam) -> reduction (substitute s_lam x s2)
    | _ ->
      let new_s = LApp(reduction s1, reduction s2) in
      if s <> new_s then
        reduction new_s
      else
        new_s
  end
  | _ -> s

and substitute s x v = match s with
  | LVar(str) -> if str = x then v else s
  | LApp(s1, s2) -> LApp(substitute s1 x v, substitute s2 x v)
  | LLam(y, s1) -> LLam(y, substitute s1 x v)
  | LStat(s1, s2, s3) -> LStat(substitute s1 x v, substitute s2 x v, substitute s3 x v)
  | LNot(s1) -> LNot(substitute s1 x v)
  | LAnd(s1, s2) -> LAnd(substitute s1 x v, substitute s2 x v)
  | LOr(s1, s2) -> LOr(substitute s1 x v, substitute s2 x v)
  | LOption(s1) -> LOption(substitute s1 x v)

  | LInit s -> LInit(substitute s x v)
  | LExists s -> LExists(substitute s x v)
  | LThe(s1, s2) -> LThe(substitute s1 x v, substitute s2 x v)
  | LThing -> LThing
  | LTrue -> LTrue
  | LBind(s1, s2) -> LBind(substitute s1 x v, substitute s2 x v)
  | LWhere(s1, s2) -> LWhere(substitute s1 x v, substitute s2 x v)
  | LEq(s1, s2) -> LEq(substitute s1 x v, substitute s2 x v)
