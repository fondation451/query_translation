(* PARSER for the SQUALL controlled language *)

%{

  open Parsing;;
  open Squall_ast;;

  let loc () = symbol_start_pos (), symbol_end_pos ();;

%}

%token EOF
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
%left AND
%left OR
%left MAYBE
%left NOT


/* Point d'entre */

%start sentence
%type <Squall_ast.lambda_ast> sentence

%%

sentence:
  |delta(np) delta(vp) EOF {mk_t (LApp($1, $2)) S_tag}
;

np:
  TERM
  {
    let x = mk_var () in
    mk_tn (LLam(x, mk_tn (LApp(mk_tn (LVar(x)), mk_t (LVar($1)) NP_tag))))
  }
;

vp:
  |delta(p1)
  {
    let x = mk_var () in
    mk_t (LLam(x, mk_tn (LApp($1, mk_tn (LVar(x)))))) VP_tag
  }
  |delta(p2) delta(np)
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

delta(syntagm):
  |NOT d1 = delta(syntagm) { mk_tn (LNot(d1)) }
  |d1 = delta(syntagm) AND d2 = delta(syntagm) { mk_tn (LAnd(d1, d2)) }
  |d1 = delta(syntagm) OR d2 = delta(syntagm) { mk_tn (LOr(d1, d2)) }
  |MAYBE d1 = delta(syntagm) { mk_tn (LOption(d1)) }
  |syntagm { $1 }
;
