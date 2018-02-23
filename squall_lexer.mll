(* LEXER for the SQUALL controlled language *)

{

  open Lexing;;
  open Squall_parser;;

  exception Lexing_error of string;;

  let id_or_keyword =
    let h = Hashtbl.create 17 in
    List.iter (fun (s,k) -> Hashtbl.add h s k) [
      (*
      "not", NOT;
      "and", AND;
      "or", OR;
      "maybe", MAYBE;
      *)
      "of", OF;
      "a", A;
      "an", A;
      "the", THE;
      "thing", THING;
      "that", THAT;
      "such", SUCH;
      "which", WHICH;
      "whose", WHOSE;
      "is", BE;
      "are", BE;
      "have", HAVE;
      "has", HAVE;
      "where", WHERE;
      "whether", WHETHER;
      "what", WHAT;
      "how", HOW;
      "many", MANY;
      "some", SOME;
      "every", EVERY;
      "no", NO;
      "at", AT;
      "least", LEAST;
      "for", FOR;
      "there", THERE
      (*
      "in", IN;
      "graph", GRAPH
      *)
    ];
    fun s -> try Hashtbl.find h s with Not_found -> TERM s
  ;;

  let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- {pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum}
  ;;

}

let alpha = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let ident = alpha (alpha | '_' | digit)*

rule token = parse
  | '\n'
    { newline lexbuf; token lexbuf }
  | [' ' '\t' '\r']+
    { token lexbuf}
  | ','
    { COMA }
  | ident
    { id_or_keyword (lexeme lexbuf) }
  | '0' | ['1'-'9'] digit* as num
    {
      let i =
      try int_of_string num
      with _ -> raise (Lexing_error "int overflow") in
      if i > (1 lsl 31)-1 || i < -(1 lsl 31)
      then raise (Lexing_error "int overflow")
      else INTEGER(i)
}
  | eof {EOF}
  | _
    { raise (Lexing_error (lexeme lexbuf)) }
