(* PARSER for the SQUALL controlled language *)

%{

  open Parsing;;
  open Squall_ast;;

  let loc () = symbol_start_pos (), symbol_end_pos ();;

%}

%token EOF NOT AND OR MAYBE OF A THE THING THAT SUCH WHICH WHOSE IS HAVE WHERE
%token WHETHER WHAT HOW MANY SOME EVERY NO AT LEAST FOR THERE IN GRAPH
%token <string> TERM

%left WHERE
%left AND
%left OR
%left MAYBE
%left NOT

%start sentence
%type <Squall_ast.lambda_ast> sentence

%%

sentence:
  | delta(np) delta(vp) EOF { LApp($1, $2) }
;

np:
  TERM
  {
    let x = mk_var () in
    LLam(x, (LApp(LVar x, LVar $1)))
  }
;

vp:
  | delta(p1)
  {
    let x = mk_var () in
    LLam(x, LApp($1,  LVar(x)))
  }
  | delta(p2) delta(np)
  {
    let x = mk_var () in
    let y = mk_var () in
    LLam(x, LApp($2, LLam(y, LApp(LApp($1, LVar x), LVar y))))
  }
;

p1:
  | TERM
  {
    let x = mk_var () in
    LLam(x, LStat(LVar x, LVar("rdf:type"), LVar $1))
  }
;

p2:
  | TERM
  {
    let x = mk_var () in
    let y = mk_var () in
    LLam(x, LLam(y, LStat(LVar x, LVar $1, LVar y)))
  }
;

delta(syntagm):
  | NOT d1 = delta(syntagm) { LNot(d1) }
  | d1 = delta(syntagm) AND d2 = delta(syntagm) { LAnd(d1, d2) }
  | d1 = delta(syntagm) OR d2 = delta(syntagm) { LOr(d1, d2) }
  | MAYBE d1 = delta(syntagm) { LOption(d1) }
  | syntagm { $1 }
;
