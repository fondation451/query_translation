(* Main *)

open Printf
open Lexing
open Format

let parse_only = ref false
let verbose = ref false

let input_file = ref ""
let output_file = ref ""

let usage = "usage: squallc [options] file.txt"

let report_loc (b, e) =
  let l = b.pos_lnum in
  let fc = b.pos_cnum - b.pos_bol + 1 in
  let lc = e.pos_cnum - b.pos_bol + 1 in
  eprintf "Line %d, characters %d-%d:\n" l fc lc

let report_loc_with_marker offset (b, e) =
  let fc = b.pos_cnum - b.pos_bol + 1 in
  let lc = e.pos_cnum - b.pos_bol + 1 in
  let marker = Bytes.make (lc + offset) ' ' in
  Bytes.fill marker (fc + offset) (lc - fc) '^';
  eprintf "%s\n" (Bytes.to_string marker)

let set_file f s = f := s

let options = [
  "-v", Arg.Set verbose, "  Verbose mode"
]

let file_to_string file =
  let f = open_in file in
  let rec loop out =
    try
      loop (out ^ (input_line f))
    with End_of_file -> out
  in
  let out = loop "" in
  close_in f;
  out

let process buf =
  let s = Squall_parser.parse_sentence Squall_lexer.token buf in
  if !verbose then  begin
    Printf.printf "(********** Lambda **********)\n%s\n\n"
    (Squall_ast.show_lambda_ast s)
  end;
  let s_reduced = Squall_rewriting.beta_reduce s in
  if !verbose then  begin
  Printf.printf "(********** Reduced **********)\n%s\n\n"
    (Squall_ast.show_lambda_ast s_reduced)
  end;
  let s_sugar = To_sparql.remove_sugar s_reduced in
  if !verbose then  begin
  Printf.printf "(********** Sugar Removed **********)\n%s\n\n"
    (Squall_ast.show_lambda_ast s_sugar)
  end;
  let query_ast = To_sparql.to_sparql s_sugar in
  if !verbose then  begin
  Printf.printf "(********** QUERY AST **********)\n%s\n\n"
    (Sparql_ast.show_request query_ast)
  end;
  Sparql_to_str.to_str query_ast

let () =
  Arg.parse options (set_file input_file) usage;

  if !input_file = "" then begin
    Printf.printf "Starting interactive mode.\n";
    while true do
      Printf.printf ">>> ";
      let query = read_line() in
      let buf = Lexing.from_string query in
      try let rq = process buf in
        Printf.printf "\n%s\n" rq
      with
      | Squall_lexer.Lexing_error(str) ->
        report_loc_with_marker 3 (lexeme_start_p buf, lexeme_end_p buf);
        eprintf "Lexing error : %s\n@." str;
      | Squall_parser.Error ->
        report_loc_with_marker 3(lexeme_start_p buf, lexeme_end_p buf);
        eprintf "Syntax error\n@.";
      | _ ->
        eprintf "Compilation error\n@.";
    done;
  end
  else begin
    if not (Filename.check_suffix !input_file ".txt") then begin
      eprintf "The input file must be a .txt\n@?";
      Arg.usage options usage;
      exit 1
    end;

    let baseName = Filename.remove_extension !input_file in
    let outputName = baseName ^ ".rq" in
    let buf = Lexing.from_string (file_to_string !input_file) in

    try
      let rq = process buf in
      let out = open_out outputName in
      Printf.fprintf out "%s\n" rq;
      exit 0
    with
    | Squall_lexer.Lexing_error(str) ->
      report_loc (lexeme_start_p buf, lexeme_end_p buf);
      eprintf "Lexing error : %s\n@." str;
      exit 1
    | Squall_parser.Error ->
      report_loc (lexeme_start_p buf, lexeme_end_p buf);
      eprintf "Syntax error\n@.";
      exit 1
    | _ ->
      eprintf "Compilation error\n@.";
  end
