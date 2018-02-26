(* SQUALL To SPARQL transformation *)

open Squall_ast;;
open Sparql_ast;;

exception Not_A_Request of string;;

let retrieve_var t =
  match t with
  |LVar(out) -> out
  |_ -> raise (Not_A_Request "Not a Variable")
;;

let rec remove_sugar t =
  match t with
  (* Les init servent juste pour les a barres... *)
  |LInit(t1) -> remove_sugar t1
  |LStat(x, p, y) ->
    let t = mk_var () in
    LExists(
      LLam(
        t,
        List.fold_left
          (fun out (uri, z) -> LAnd(out, LTriple(LVar(t), LVar(uri), z)))
          LTrue
          [("rdf:subject", x) ; ("rdf:predicate", p) ; ("rdf:object", y)]))
  (* LTriple(remove_sugar x, remove_sugar p, remove_sugar y) *)
  |LVar(_) -> t
  |LInt(_) -> t
  |LApp(t1, t2) -> LApp(remove_sugar t1, remove_sugar t2)
  |LLam(str, t1) -> LLam(str, remove_sugar t1)
  |LNot(t1) -> LNot(remove_sugar t1)
  |LAnd(t1, t2) -> LAnd(remove_sugar t1, remove_sugar t2)
  |LOr(t1, t2) -> LOr(remove_sugar t1, remove_sugar t2)
  |LOption(t1) -> LOption(remove_sugar t1)
  |LExists(t1) -> LExists(remove_sugar t1)
  |LThe(t1, t2) -> LThe(remove_sugar t1, remove_sugar t2)
  |LTrue -> LTrue
  |LBind(t1, t2) -> LBind(remove_sugar t1, remove_sugar t2)
  |LWhere(t1, t2) -> LWhere(remove_sugar t1, remove_sugar t2)
  |LEq(t1, t2) -> LPred2(Pred_EQ, remove_sugar t1, remove_sugar t2)
  |LAsk(t1) -> LAsk(remove_sugar t1)
  |LSelect(t1) -> LSelect(remove_sugar t1)
  |LCount(t1) ->
    let x = mk_var () in
    LSelect(LLam(x, LAggreg("COUNT", LVar(x), remove_sugar t1, [])))
  |LForall(t1, t2) -> LForall(remove_sugar t1, remove_sugar t2)
  |LAtleast(i, t1) ->
    let x = mk_var () in
    LExists(LLam(x, LAnd(LAggreg("COUNT", LVar(x), remove_sugar t1, []), LPred2(Pred_GE, LVar(x), LInt(i)))))

  (* Mes Rajouts *)
  |LTriple(t1, t2, t3) -> LTriple(remove_sugar t1, remove_sugar t2, remove_sugar t3)
  |LPred2(pred, t1, t2) -> LPred2(pred, remove_sugar t1, remove_sugar t2)
  |LPred1(pred, t1) -> LPred1(pred, remove_sugar t1)
  |LAggreg(agg, t1, t2, t_l) -> LAggreg(agg, remove_sugar t1, remove_sugar t2, List.map remove_sugar t_l)

;;

let rec compile_request t =
  let t = Squall_rewriting.beta_reduce t in
  Printf.printf "(++++++++ COMPILE REQUEST ++++++++)\n%s\n"
    (Squall_ast.show_lambda_ast t);
  match t with
  |LAsk(f) -> ASK(compile_graph f)
  |LSelect(d) ->
    let v = mk_var () in
    compile_request_l [v] (LApp(d, LVar(v)))
  |f ->
    let i, d, g = compile_update f in
    INS_DEL_UPD(i, d, g)

and compile_request_l v_l t =
  let t = Squall_rewriting.beta_reduce t in
  Printf.printf "(++++++++ COMPILE REQUEST_L !!! ++++++++)\n%s\n"
    (Squall_ast.show_lambda_ast t);
  match t with
  |LSelect(d) ->
    let v = mk_var () in
    compile_request_l (v::v_l) (LApp(d, LVar(v)))
  |f -> SELECT(List.rev v_l, compile_graph f)

and compile_graph f =
  let f = Squall_rewriting.beta_reduce f in
  Printf.printf "(|||||||| COMPILE GRAPH ||||||||)\n%s\n"
    (Squall_ast.show_lambda_ast f);
  match f with
  |LTriple(s, p, o) -> GTRIPLET(retrieve_var s, retrieve_var p, retrieve_var o)
  |LBind(x, y) -> GBIND(retrieve_var x, retrieve_var y)
  |LPred1(pred, x) -> GFILTER(pred, [retrieve_var x])
  |LPred2(pred, x, y) -> GFILTER(pred, [retrieve_var x ; retrieve_var y])
(*  |LGraph(x, s, a, g) -> GGRAPH(retrieve_var x, compile_graph (LApp(LApp(s, unit_val), true_fun)))*)
  |LAggreg(agg, x, d, di_l) ->
    let zi_l = List.map (fun di -> mk_var ()) di_l in
    let y = mk_var () in
(*    GSAWGB(
      zi_l, agg, y, retrieve_var x,
      compile_graph (List.fold_left2 (fun out di zi -> LAnd(out, LApp(di, LVar(zi)))) (LApp(d, LVar(y))) di_l zi_l)
    )*)
    GSAWGB(
      zi_l, agg, y, retrieve_var x,
      compile_graph (List.fold_left2 (fun out di zi -> LAnd(out, LApp(di, LVar(zi)))) d di_l zi_l)
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
  |_ -> raise (Not_A_Request "Compile_graph")

and compile_update f =
  let f = Squall_rewriting.beta_reduce f in
  Printf.printf "(-------- COMPILE UPDATE --------)\n%s\n"
    (Squall_ast.show_lambda_ast f);
  match f with
  |LTriple(s, p, o) -> GTRIPLET(retrieve_var s, retrieve_var p, retrieve_var o), GEPSILON, GEPSILON
  |LTrue -> GEPSILON, GEPSILON, GEPSILON
  |LAnd(f1, f2) ->
    let i1, d1, g1 = compile_update f1 in
    let i2, d2, g2 = compile_update f2 in
    GSEQUENCE(i1, i2), GSEQUENCE(d1, d2), GSEQUENCE(g1, g2)
  |LNot(f1) ->
    let i1, d1, g1 = compile_update f1 in
    d1, i1, g1
  |LWhere(f1, f2) ->
    let i1, d1, g1 = compile_update f1 in
    i1, d1, GSEQUENCE(g1, compile_graph f2)
  |LExists(f1) -> compile_update (LApp(f1, LVar(mk_var ())))
  |LForall(f1, f2) ->
    let v = mk_var () in
    compile_update (LWhere((LApp(f2, LVar(v))), (LApp(f1, LVar(v)))))
  |LThe(f1, f2) ->
    let v = mk_var () in
    compile_update (LWhere((LApp(f2, LVar(v))), (LApp(f1, LVar(v)))))
  |LSelect(_) -> (compile_request f, GEPSILON, GEPSILON)
  |_ -> raise (Not_A_Request "Compile_update")
;;
