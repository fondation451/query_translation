(* SPARQL AST *)

type ident = string

and pred = Squall_ast.pred

and agg = string

and prefix = ident * ident

and graph_pattern =
  |GTRIPLET of ident * ident * ident
  |GBIND of ident * ident
  |GFILTER of pred * ident list
  |GGRAPH of ident * graph_pattern
  |GSAWGB of ident list * agg * ident * ident * graph_pattern (* SELECT AS WHERE GROUP BY *)
  |GEPSILON
  |GUNION of graph_pattern * graph_pattern
  |GOPTIONAL of graph_pattern
  |GFILTER_NE of graph_pattern
  |GSEQUENCE of graph_pattern * graph_pattern
  |ASK of graph_pattern
  |INS_DEL_UPD of graph_pattern * graph_pattern * graph_pattern
  |SELECT of ident list * graph_pattern

[@@deriving show { with_path = false }];;
