(* PARSER for the SQUALL controlled language *)

%{

  open Parsing
  open Squall_ast

  let loc() = symbol_start_pos(), symbol_end_pos()

%}

(* TODO: Fix ambiguities *)
(* TODO: Still not implemented *)
(* %token WHETHER WHAT THERE SOME HOW MANY EVERY NO AT LEAST FOR IN GRAPH *)

(* Misc *)
%token THING EOF

(* Determinants *)
%token A THE

(* Verbs *)
%token BE HAVE

(* TODO: Relational *)
(* %token NOT AND OR MAYBE *)

(* Relations *)
%token THAT SUCH WHICH WHOSE OF WHERE

%token <string> TERM

%start parse_sentence
%type <Squall_ast.lambda_ast> parse_sentence

%%

parse_sentence:
  s=sentence EOF { s }


sentence:
  | np=noun_phrase vp=verb_phrase
    (* np vp *)
    { LApp(np, vp) }
  | s1=sentence WHERE s2=sentence { LWhere(s1, s2) }

noun_phrase:
  | TERM
    (* λd.(d term) *)
    {
      let x = mk_var() in
      LLam(x, (LApp(LVar x, LVar $1)))
    }
  | det=determinant ng1=nominal_group1
    (* λd.(det (init ng1 ) d) *)
    {
      let d = mk_var() in
      LLam(d,
        (LApp(
          LApp(det, LInit ng1), LVar d)))
    }
  | det=determinant ng2=nominal_group2 OF np=noun_phrase
    (* λd.(np λx.(det (init (ng2 x)) d)) *)
    {
      let d = mk_var() in
      let x = mk_var() in
      let init_ng2_x = LInit(LApp(ng2, LVar x)) in
      LLam(d,
        (LApp(
          LApp(
            np,
            LLam(x, LApp(det, init_ng2_x))),
          LVar d)))
    }

determinant:
  | A
    (* λd1.λd2.(exBEts (and d1 d2 )) *)
    {
      let d1 = mk_var() in
      let d2 = mk_var() in
      LLam(d1, LLam(d2, LExists(LAnd(LVar d1, LVar d2))))
    }
  | THE
    (* λd1.λd2.(the d1 d2 ) *)
    {
      let d1 = mk_var() in
      let d2 = mk_var() in
      LLam(d1, LLam(d2, LThe(LVar d1, LVar d2)))
    }

nominal_group1:
  | THING ar=appositions_and_relatives
    (* and thing ar *)
    { LAnd(LThing, ar) }
  | p1=unary_predicate ar=appositions_and_relatives
    (* and p1 ar *)
    { LAnd(p1, ar) }

nominal_group2:
  p2=binary_predicate ar=appositions_and_relatives
    (* λx.λy.(and (p2 x y) (ar y)) *)
    {
      let x = mk_var() in
      let y = mk_var() in
      LLam(x, LLam(y,
      LAnd(LApp(LApp(p2, LVar x), LVar y), LApp(ar, LVar y))))
    }


appositions_and_relatives:
  | app=apposition rel=relative
    (* and app rel *)
    { LAnd(app, rel) }
  | app=apposition
    (* app *)
    { app }

apposition:
  (* TODO: uri case *)
  (* TODO: Var case *)
  |
    (* λx.true *)
    {
      let x = mk_var() in
      LLam(x, LTrue)
    }

relative:
  | THAT vp=verb_phrase
    (* init vp *)
    { LInit vp }
  | THAT np=noun_phrase p2=binary_predicate
    (* init λx.(np λy.(p2 y x)) *)
    {
      let x = mk_var() in
      let y = mk_var() in
      LInit(LLam(x, LApp(np, LLam(y, LApp(LApp(p2, LVar y), LVar x)))))
    }
  | SUCH THAT s=sentence
    (* init λx.s *)
    {
      let x = mk_var() in
      LInit(LLam(x, s))
    }
  | det=determinant ng2=nominal_group2 OF WHICH vp=verb_phrase
    (* init λx.(det (ng2 x) vp) *)
    {
      let x = mk_var() in
      LInit(LLam(x, LApp(LApp(det, LApp(ng2, LVar x)), vp)))
    }
  | WHOSE ng2=nominal_group2 vp=verb_phrase
    (* ≡ the NG2 of which VP *)
    {
      let thelambda =
        let d1 = mk_var() in
        let d2 = mk_var() in
        LLam(d1, LLam(d2, LThe(LVar d1, LVar d2)))
      in
      let x = mk_var() in
      LInit(LLam(x, LApp(LApp(thelambda, LApp(ng2, LVar x)), vp)))
    }
  | WHOSE p2=binary_predicate BE np=noun_phrase
    (* λx.(np λy.(p2 x y)) *)
    {
      let x = mk_var() in
      let y = mk_var() in
      LLam(x, LApp(np, LLam(y, LApp(LApp(p2, LVar x), LVar y))))
    }

unary_predicate:
  | TERM
  {
    let x = mk_var () in
    LLam(x, LStat(LVar x, LVar("rdf:type"), LVar $1))
  }
;

binary_predicate:
  | TERM
  {
    let x = mk_var () in
    let y = mk_var () in
    LLam(x, LLam(y, LStat(LVar x, LVar $1, LVar y)))
  }

verb_phrase:
  | p1=unary_predicate
    (* λx.(p1 x) *)
    {
      let x = mk_var() in
      LLam(x, LApp(p1,  LVar x))
    }
  | p2=binary_predicate np=noun_phrase
    (* λx.(np λy.(p2 x y)) *)
    {
      let x = mk_var() in
      let y = mk_var() in
      LLam(x, LApp(np, LLam(y, LApp(LApp(p2, LVar x), LVar y))))
    }
  | BE apOrRel=auxiliary_verb
  | BE apOrRel=relative
    (* ap or rel *)
    { apOrRel }
  | HAVE det=determinant p2=binary_predicate ar=appositions_and_relatives
    (* λx.(det (p2 x) ar) *)
    {
      let x = mk_var() in
      LLam(x, LApp(LApp(det, LApp(p2, LVar x)), ar))
    }

auxiliary_verb:
  | term=TERM
    (* λx.(eq x term) *)
    {
      let x = mk_var() in
      LLam(x, LEq(LVar x, LVar term))
    }
  | A ng1=nominal_group1
  | THE ng1=nominal_group1
    (* ng1 *)
    { ng1 }
  | A ng2=nominal_group2 OF np=noun_phrase
  | THE ng2=nominal_group2 OF np=noun_phrase
    (* λx.(np λy.(ng2 y x)) *)
    {
        let x = mk_var() in
        let y = mk_var() in
        LLam(x, LApp(np, LLam(y, LApp(LApp(ng2, LVar y), LVar x))))
    }
