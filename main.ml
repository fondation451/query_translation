(* Main *)

open Printf;;
open Lexing;;
open Format;;

let parse_only = ref false;;
let verbose = ref false;;

let input_file = ref "";;
let output_file = ref "";;

let usage = "usage: scala [options] file.lus";;

let set_file f s = f := s;;

let options = [
  "--parse-only", Arg.Set parse_only, "  Execute only syntactic analysis";
  "-v", Arg.Set verbose, "  Verbose mode"
];;

let localisation pos =
  let l = pos.pos_lnum in
  let c = pos.pos_cnum - pos.pos_bol + 1 in
  eprintf "File \"%s\", line %d, characters %d-%d:\n" !input_file l (c-1) c
;;

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
;;

let string_of_char c = String.make 1 c;;

let preprocess_str str =
  let len = String.length str in
  let punctuation = ['.' ; ',' ; ';' ; '?' ; '!' ; '"'] in
  let rec loop i out =
    if i >= len then
      out
    else if List.mem (String.get str i) punctuation then
      loop (i+1) out
    else
      loop (i+1) (out ^ (string_of_char (String.get str i)))
  in loop 0 ""
;;

let () =
  Arg.parse options (set_file input_file) usage;

  if !input_file = "" then begin
    eprintf "No file to compile\n@?";
    Arg.usage options usage;
    exit 1
  end;

  if not (Filename.check_suffix !input_file ".txt") then begin
    eprintf "The input file must be a .txt\n@?";
    Arg.usage options usage;
    exit 1
  end;

  let f_name = Filename.remove_extension !input_file in

  let content =
    !input_file
    |> file_to_string
    |> preprocess_str
  in

  let buf = Lexing.from_string content in

  try
    let p = Squall_parser.sentence Squall_lexer.token buf in
    exit 0
  with
  |Squall_lexer.Lexical_error(str) ->
    (localisation (Lexing.lexeme_start_p buf);
    eprintf "Lexing error@.";
    Printf.printf "%s\n" str;
    exit 1)
;;