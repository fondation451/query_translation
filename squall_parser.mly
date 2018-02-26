(* PARSER for the SQUALL controlled language *)

%{

  open Parsing
  open Squall_ast

  let loc() = symbol_start_pos(), symbol_end_pos()

%}

(* TODO: Fix ambiguities *)
(* TODO: Still not implemented *)
(* %token IN GRAPH *)

(* Misc *)
%token THING COMA EOF

(* Queries *)
%token WHETHER WHAT HOW MANY

(* Quantifiers *)
%token SOME EVERY NO AT LEAST THERE FOR

(* Determinants *)
%token A THE

(* Verbs *)
%token BE HAVE

(* Relational *)
%token NOT AND OR MAYBE

(* Relations *)
%token THAT SUCH WHICH WHOSE OF WHERE

%token <int> INTEGER
%token <string> TERM
%token <string> CLASS
%token <string> PROPERTY

%start parse_sentence
%type <Squall_ast.lambda_ast> parse_sentence

%left COMA
%left WHETHER
%left THAT
%left WHERE


%left OR
%left AND
%left NOT

%%

parse_sentence:
  s=sentence EOF { s }


sentence:
  | np=delta(noun_phrase) vp=delta(verb_phrase)
    (* np vp *)
    { LApp(np, vp) }
  | s1=sentence WHERE s2=sentence
    (*  where s1 s2 *)
    { LWhere(s1, s2) }
  | WHETHER s=sentence
    (* ask s *)
    { LAsk s }
  | FOR np=noun_phrase COMA s=sentence
    (* np λx.s *)
    {
      let x = mk_var() in
      LApp(np, LLam(x, s))
    }
  | THERE BE np=noun_phrase
    (* np λx.true *)
    {
      let x = mk_var() in
      LApp(np, LLam(x, LTrue))
    }

noun_phrase:
  | term=TERM
    (* λd.(d term) *)
    {
      let x = mk_var() in
      LLam(x, (LApp(LVar x, LVar term)))
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
  | WHAT
    (* ≡ which thing *)
    {
      let d = mk_var() in
      LLam(d, LSelect(LAnd(thing, LVar d)))
    }
(* TODO:
    | WHOSE ng2=nominal_group2
    (* ≡ the ng2 of what *)
    {

    }
*)

determinant:
  | A | SOME
    (* λd1.λd2.(exists (and d1 d2 )) *)
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
  | WHICH
    (* λd1.λd2.(select (and d1 d2)) *)
    {
      let d1 = mk_var() in
      let d2 = mk_var() in
      LLam(d1, LLam(d2, LSelect(LAnd(LVar d1, LVar d2))))
    }
  | HOW MANY
    (*λd1.λd2.(count (and d1 d2)) *)
    {
      let d1 = mk_var() in
      let d2 = mk_var() in
      LLam(d1, LLam(d2, LCount(LAnd(LVar d1, LVar d2))))
    }
  | EVERY
    (* λd1.λd2.(forall d1 d2) *)
    {
      let d1 = mk_var() in
      let d2 = mk_var() in
      LLam(d1, LLam(d2, LForall(LVar d1, LVar d2)))
    }
  | NO
    (* λd1.λd2.(not (exists (and d1 d2))) *)
    {
      let d1 = mk_var() in
      let d2 = mk_var() in
      LLam(d1, LLam(d2, LNot(LExists(LAnd(LVar d1, LVar d2)))))
    }
  | AT LEAST i=INTEGER
    (* λd1.λd2.(atleast i (and d1 d2)) *)
    {
      let d1 = mk_var() in
      let d2 = mk_var() in
      LLam(d1, LLam(d2, (LAtleast(i, LAnd(LVar d1, LVar d2)))))
    }


nominal_group1:
  | THING ar=appositions_and_relatives
    (* and thing ar *)
    { LAnd(thing, ar) }
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
  | uri=PROPERTY
  | uri=CLASS
    (* λx.(eq x uri) *)
    {
      let x = mk_var() in
      LEq(LVar x, LVar uri)
    }
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
  | uri=CLASS
  (* λx.(type x uri ) *)
  {
    let x = mk_var () in
    LLam(x, LStat(LVar x, LVar("rdf:type"), LVar uri))
  }
;

binary_predicate:
  | uri=PROPERTY
  (* λx.λy.(stat x uri y) *)
  {
    let x = mk_var () in
    let y = mk_var () in
    LLam(x, LLam(y, LStat(LVar x, LVar uri, LVar y)))
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

delta(syntagm):
  | d1 = delta(syntagm) OR d2 = delta(syntagm) { LOr(d1, d2) }
  | d1 = delta(syntagm) AND d2 = delta(syntagm) { LAnd(d1, d2) }
  | NOT d1 = delta(syntagm) { LNot(d1) }
  | MAYBE d1=syntagm { LOption(d1) }
  | syntagm { $1 }
