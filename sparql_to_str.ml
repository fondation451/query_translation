(* Sparql to String *)

open Squall_ast;;
open Sparql_ast;;

let (<<<) = Buffer.add_string;;

let pred_to_string pred =
  match pred with
  |Pred_EQ -> "=="
  |Pred_LT -> "<"
  |Pred_LE -> "<="
  |Pred_GT -> ">"
  |Pred_GE -> ">="
;;

let agg_to_string agg = agg;;

let rec from_request r buf =
  match r with
  |ASK(g) ->
    buf <<< "ASK {\n";
    from_graph g buf;
    buf <<< "}\n"
  |INS_DEL_UPD(u) ->
    let i, d, g = u in
    buf <<< "INSERT {\n";
    from_graph i buf;
    buf <<< "}\nDELETE {\n";
    from_graph d buf;
    buf <<< "}\nWHERE {\n";
    from_graph g buf;
    buf <<< "}\n"
  |SELECT(v_l, g) ->
    buf <<< "SELECT ";
    List.iter (fun v -> buf <<< v; buf <<< " ") v_l;
    buf <<< "WHERE {\n";
    from_graph g buf;
    buf <<< "}\n"

and from_graph g buf =
  match g with
  |GTRIPLET(i1, i2, i3) -> buf <<< (i1 ^ " " ^ i2 ^ " " ^ i3 ^ ".\n")
  |GBIND(i1, i2) -> buf <<< ("BIND ( " ^ i1 ^ " AS " ^ i2 ^ ")\n")
  |GFILTER(pred, i_l) ->
    buf <<< ("FILTER " ^ (pred_to_string pred) ^ "(");
    let rec loop = function |[] -> () |[i] -> buf <<< (i ^ ")\n") |i::l' -> buf <<< (i ^ ", "); loop l' in loop i_l
  |GGRAPH(i1, g1) -> assert false
  |GSAWGB(z_l, agg, y, x, g1) ->
    buf <<< "{ SELECT ";
    List.iter (fun zi -> buf <<< zi; buf <<< " ") z_l;
    buf <<< "( ";
    buf <<< (agg_to_string agg);
    buf <<< "(";
    buf <<< y;
    buf <<< ") AS ";
    buf <<< x;
    buf <<< " \nWHERE {\n";
    from_graph g1 buf;
    buf <<< "}\nGROUP BY ";
    List.iter (fun zi -> buf <<< zi; buf <<< " ") z_l
  |GEPSILON -> ()
  |GUNION(g1, g2) ->
    buf <<< "{\n";
    from_graph g1 buf;
    buf <<< "}\nUNION {\n";
    from_graph g2 buf;
    buf <<< "}\n"
  |GOPTIONAL(g1) ->
    buf <<< "OPTIONAL {\n";
    from_graph g1 buf;
    buf <<< "}\n"
  |GFILTER_NE(g1) ->
    buf <<< "FILTER NOT EXISTS {\n";
    from_graph g1 buf;
    buf <<< "}\n"
  |GSEQUENCE(g1, g2) ->
    from_graph g1 buf;
    from_graph g2 buf
;;

let to_str q =
  let buf = Buffer.create 10 in
  from_request q buf;
  Buffer.contents buf
;;