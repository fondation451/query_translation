(* LEXER for the SQUALL controlled language *)

{

  open Lexing;;
  open Parser;;

  exception Lexical_error of string;;

  let id_or_keyword =
    let h = Hashtbl.create 17 in
    List.iter (fun (s,k) -> Hashtbl.add h s k) [
      "not", NOT;
      "and", AND;
      "or", OR;
      "maybe", MAYBE;
      "of", OF;
      "a", A;
      "an", A;
      "the", THE;
      "thing", THING;
      "that", THAT;
      "such", SUCH;
      "which", WHICH;
      "whose", WHOSE;
      "is", IS;
      "are", IS;
      "have", HAVE;
      "has", HAVE;
      "where", WHERE;
      "whether", WHETHER;
      "thing", THING;
      "what", WHAT;
      "how", HOW;
      "many", MANY;
      "some", SOME;
      "every", EVERY;
      "no", NO;
      "at", AT;
      "least", LEAST;
      "for", FOR;
      "there", THERE;
      "in", IN;
      "graph", GRAPH
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
  |'\n'
    {newline lexbuf; token lexbuf}
  |[' ' '\t' '\r']+
    {token lexbuf}
  |ident
    {id_or_keyword (lexeme lexbuf)}
  |_
    {raise (Lexical_error (lexeme lexbuf))}
