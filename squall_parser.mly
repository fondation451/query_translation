(* PARSER for the SQUALL controlled language *)

%{

  open Parsing;;
  open Squall_ast;;

  let loc () = symbol_start_pos (), symbol_end_pos ();;
  let mk_t t sem = (t, sem);;
  let mk_tn t = (t, None);;

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

sentence: np vp {mk_tn (Var("lal"))}(*{mk_tn (App($1, $2))}*)
;

np:
  TERM {let x = mk_var () in mk_tn (Lam(x, App(mk_tn (Var(x)), mk_t (Var($1)) Term)))}
;

vp:
  |p1 {let x = mk_var () in mk_tn (Lam(x, App($1, mk_tn (Var(x)))))}
  |p2 np
  {
    let x = mk_var () in mk_tn (Var(x))
(*    let y = mk_var () in
    mk_tn
      (Lam(
        x,
        App(
          mk_t (Var($2)) NP,
          mk_tn
            (Lam(
              y,
              App(
                mk_tn
                  (App(
                    mk_t (Var($1)) P2,
                    mk_tn (Var(x)))),
                mk_tn (Var(y))))))))*)
  }
;

p1:
  |TERM {let x = mk_var () in mk_t (Lam(x, Stat(mk_tn (Var(x)), mk_tn (Var("rdf:type")), mk_t (Var($1)) Term))) P1}
;

p2:
  |TERM
  {
    let x = mk_var () in
    let y = mk_var () in
    mk_t (Lam(x, Lam(y, Stat(mk_tn (Var(x)), mk_t (Var($1)) Term, mk_tn (Var(y)))))) P2
  }
;
