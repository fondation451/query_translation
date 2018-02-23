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

let process query =
  let buf = Lexing.from_string query in
  try
    buf
    |> Squall_parser.parse_sentence Squall_lexer.token
    |> (fun s -> printf "(********** Lambda **********)\n%s\n" (Squall_ast.show_lambda_ast s); s)
    |> Squall_rewriting.beta_reduce
    |> (fun s -> printf "(********** Reduced **********)\n%s\n\n" (Squall_ast.show_lambda_ast s); s)
    |> (fun s -> ())
  with
  | Squall_lexer.Lexing_error(str) ->
    report_loc_with_marker 3 (lexeme_start_p buf, lexeme_end_p buf);
    eprintf "Lexing error : %s\n@." str;
  | Squall_parser.Error ->
    report_loc_with_marker 3(lexeme_start_p buf, lexeme_end_p buf);
    eprintf "Syntax error\n@.";
;;

let () =
  Arg.parse options (set_file input_file) usage;

  if !input_file = "" then begin
    Printf.printf "Starting interactive mode.\n";
    while true do
      Printf.printf ">>> ";
      process (read_line ())
    done;
  end
  else begin
    if not (Filename.check_suffix !input_file ".txt") then begin
      eprintf "The input file must be a .txt\n@?";
      Arg.usage options usage;
      exit 1
    end;
    process (file_to_string !input_file)
  end
