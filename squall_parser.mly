(* PARSER for the SQUALL controlled language *)

%{

  open Parsing;;
  open Squall_ast;;

  let loc () = symbol_start_pos (), symbol_end_pos ();;

%}

%token NOT
%token AND
%token OR
%token MAYBE
%token OF
%token A
%token THE
%token THING
%token THAT
%token SUCH
%token WHICH
%token WHOSE
%token IS
%token HAVE
%token WHERE
%token WHETHER
%token WHAT
%token HOW
%token MANY
%token SOME
%token EVERY
%token NO
%token AT
%token LEAST
%token FOR
%token THERE
%token IN
%token GRAPH
%token <string> TERM


%left WHERE
%left OR
%left AND
%left MAYBE
%left NOT


/* Point d'entre */

%start sentence
%type <Squall_ast.lambda_ast> sentence

%%

sentence:
  np vp {mk_t (LApp($1, $2)) S_tag}
;

np:
  TERM
  {
    let x = mk_var () in
    mk_tn (LLam(x, mk_tn (LApp(mk_tn (LVar(x)), mk_t (LVar($1)) NP_tag))))
  }
;

vp:
  |p1
  {
    let x = mk_var () in
    mk_t (LLam(x, mk_tn (LApp($1, mk_tn (LVar(x)))))) VP_tag
  }
  |p2 np
  {
    let x = mk_var () in
    let y = mk_var () in
    mk_t (LLam(x, mk_tn (LApp($2, mk_tn (LLam(y, mk_tn (LApp(mk_tn (LApp($1, mk_tn (LVar(x)))), mk_tn (LVar(y)))))))))) VP_tag
  }
;

p1:
  |TERM
  {
    let x = mk_var () in
    mk_t (LLam(x, mk_tn (LStat(mk_tn (LVar(x)), mk_tn (LVar("rdf:type")), mk_t (LVar($1)) Term_tag)))) P1_tag
  }
;

p2:
  |TERM
  {
    let x = mk_var () in
    let y = mk_var () in
    mk_t (LLam(x, mk_tn (LLam(y, mk_tn (LStat(mk_tn (LVar(x)), mk_t (LVar($1)) Term_tag, mk_tn (LVar(y)))))))) P2_tag
  }
;
