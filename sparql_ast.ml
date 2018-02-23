(* SPARQL AST *)

type ident = string;;

type pred =
  |Pred_EQ
  |Pred_LT
  |Pred_GT
;;

type agg = string;;

type prefix = ident * ident;;

type graph_pattern =
  |GTRIPLET of ident * ident * ident
  |GBIND of ident * ident
  |GFILTER of pred * ident list * ident
  |GGRAPH of ident * graph_pattern
  |GSAWGB of ident list * agg * ident * ident * graph_pattern (* SELECT AS WHERE GROUP BY *)
  |GEPSILON
  |GUNION of graph_pattern * graph_pattern
  |GOPTIONAL of graph_pattern
  |GFILTER_NE of graph_pattern
  |GSEQUENCE of graph_pattern * graph_pattern
;;

type update_pattern = graph_pattern list * graph_pattern list * graph_pattern list;;

type request =
  |ASK of graph_pattern
  |INS_DEL_UPD of update_pattern
  |SELECT of select_pattern
;;
