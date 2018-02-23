(* SQUALL tTo SPARQL transformation *)

open Squall_ast;;
open Sparql_ast;;

exception Not_A_Request;;

let rec compile_request t =
  match t with
  |LAsk(f) -> ASK(compile_graph f)
  |LSelect(d) ->
    let v = mk_var () in
    compile_request_l [v] (LApp(d, LVar(v)))
  |f -> INS_DEL_UPD(compile_update f)

and compile_request_l v_l t =
  match t with
  |LSelect(d) ->
    let v = mk_var () in
    compile_request_l (v::v_l) (LApp(d, LVar(v)))
  |f -> SELECT(compile_graph f)

and compile_graph f =
  match f with
  |LTriple(s, p, o) -> GTRIPLET(s, p, o)
  |LBind(x, y, a, g) -> GBIND(x, y)
  |LPred1(pred, x, a, g) -> GFILTER(pred, [x], a)
  |LPred2(pred, x, y, a, g) -> GFILTER(pred, [x;y], a)
  |LGraph(x, s, a, g) ->
    GGRAPH(
      x,
      compile_graph (LApp(LApp(s, unit_val), true_fun))
    )
  |LAggreg(agg, x, d, di_l, a, g) ->
    let zi_l = List.map (fun di -> mk_var ()) di_l in
    let y = mk_var () in
    GSAWGB(
      zi_l, agg, y, x,
      compile_graph (List.fold_left2 (fun out di zi -> LAnd(out, LApp(di, zi))) (LApp(d, LVar(y))) di_l zi_l)
    )
  |LTrue -> GEPSILON
  |LAnd(f1, f2) -> GSEQUENCE(compile_graph f1, compile_graph f2)
  |LOr(f1, f2) -> GUNION(compile_graph f1, compile_graph f2)
  |LOption(f1) -> GOPTIONAL(compile_graph f1)
  |LNot(f1) -> GFILTER_NE(compile_graph f1)
  |LWhere(f1, f2) -> compile_graph (LAnd(f1, f2))
  |LExists(f1) -> compile_graph (LApp(f1, LVar(mk_var ())))
  |LForall(f1, f2) -> compile_graph (LNot(LExists(LAnd(f1, LNot(f2)))))
  |LThe(f1, f2) -> compile_graph (LExists(LAnd(f1, f2)))

and compile_update f =
  match f with
  |LTriple(s, p, o) -> [GTRIPLET(s, p, o)], [GEPSILON], [GEPSILON]
  |LTrue -> [GEPSILON], [GEPSILON], [GEPSILON]
  |LAnd(f1, f2) ->
    let i1, d1, g1 = compile_update f1 in
    let i2, d2, g2 = compile_update f2 in
    List.append i1 i2, List.append d1 d2, List.append g1 g2
  |LNot(f1) ->
    let i1, d1, g1 = compile_update f1 in
    d1, i1, g1
  |LWhere(f1, f2) ->
    let i1, d1, g1 = compile_update f1 in
    i1, d1, List.append g1 (compile_graph f2)
  |LExists(f1) -> compile_update (LApp(f1, LVar(mk_var ())))
  |LForall(f1, f2) ->
    let v = mk_var () in
    compile_update (LWhere((LApp(f2, LVar(v))), (LApp(f1, LVar(v)))))
  |LThe(f1, f2) ->
    let v = mk_var () in
    compile_update (LWhere((LApp(f2, LVar(v))), (LApp(f1, LVar(v)))))
  |_ -> raise Not_A_Request
;;
