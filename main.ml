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
  eprintf "%s\n" (Bytes.to_string marker);

let set_file f s = f := s

let options = [
  "--parse-only", Arg.Set parse_only, "  Execute only syntactic analysis";
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

let () =
  Arg.parse options (set_file input_file) usage;

  if !input_file = "" then begin
    Printf.printf "Starting interactive mode.\n";
    while true do
      Printf.printf ">>> ";
      let query = read_line() in
      let buf = Lexing.from_string query in
      try
        let s = Squall_parser.parse_sentence Squall_lexer.token buf in
        Printf.printf "(********** Lambda **********)\n%s\n"
          (Squall_ast.show_lambda_ast s);
        let s_reduced = Squall_rewriting.beta_reduce s in
        Printf.printf "(********** Reduced **********)\n%s\n\n"
          (Squall_ast.show_lambda_ast s_reduced);
      with
      | Squall_lexer.Lexing_error(str) ->
        report_loc_with_marker 3 (lexeme_start_p buf, lexeme_end_p buf);
        eprintf "Lexing error : %s\n@." str;
      | Squall_parser.Error ->
        report_loc_with_marker 3(lexeme_start_p buf, lexeme_end_p buf);
        eprintf "Syntax error\n@.";
    done;
  end
  else begin
    if not (Filename.check_suffix !input_file ".txt") then begin
      eprintf "The input file must be a .txt\n@?";
      Arg.usage options usage;
      exit 1
    end;

    let buf = Lexing.from_string (file_to_string !input_file) in

    try
      let s = Squall_parser.parse_sentence Squall_lexer.token buf in
      Printf.printf "(********** Lambda **********)\n%s\n"
        (Squall_ast.show_lambda_ast s);
      let s_reduced = Squall_rewriting.beta_reduce s in
      Printf.printf "(********** Reduced **********)\n%s\n\n"
        (Squall_ast.show_lambda_ast s_reduced);
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
  end
