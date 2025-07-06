(****************************************************************)
(** {3 Z3 support code}                                         *)
(****************************************************************)

(** Convert ASL expression to Z3 expression.
    This only copes with a limited set of operations: ==, +, -, * and DIV.
    (It is possible that we will need to extend this list in the future but
    it is sufficient for the current ASL specifications.)

    The support for DIV is not sound - it is a hack needed to cope with
    the way ASL code is written and generally needs a side condition
    that the division is exact (no remainder).

    ufs is a mutable list of conversions used to handle subexpressions
    that cannot be translated.  We treat such subexpressions as
    uninterpreted functions and add them to the 'ufs' list so that
    we can reason that "F(x) == F(x)" without knowing "F".
 *)

module AST = LibASL_stage0.Asl_ast
module Asl_utils = LibASL_stage0.Asl_utils

let verbose = false

let uninterp ctx ufs x sort =
  if verbose then Printf.printf "    Unable to translate %s - using as uninterpreted function\n" (Asl_utils.pp_expr x);
  match List.assoc_opt x !ufs with
  | Some uf -> uf
  | None ->
      let uf = Z3.Expr.mk_fresh_const ctx "UNINTERPRETED" sort in
      ufs := (x, uf) :: !ufs;
      uf

let mk_mul a b = AST.Expr_TApply (FIdent ("mul_int",0), [], [a; b])
let mk_add a b = AST.Expr_TApply (FIdent ("add_int",0), [], [a; b])
let mk_one = AST.Expr_LitInt "1"

(* Broadly assume division is exact. Encode this by multiplying by denominators. *)
(* TODO: Unsound, should just show division is exact. *)
let rec extract_div : AST.expr -> (AST.expr * AST.expr) = function
  | Expr_TApply (FIdent ("add_int",_), [], [a; b]) ->
      let (an,ad) = extract_div a in
      let (bn,bd) = extract_div b in
      (mk_add (mk_mul an bd) (mk_mul bn ad), mk_mul ad bd)
  | Expr_TApply (FIdent ("mul_int",_), [], [a; b]) ->
      let (an,ad) = extract_div a in
      let (bn,bd) = extract_div b in
      (mk_mul an bn, mk_mul ad bd)
  | Expr_TApply (FIdent ("fdiv_int",_), [], [a; b]) -> (a,b)
  | x -> (x,mk_one)

let rec z3_of_bool (ctx: Z3.context) (ufs: (AST.expr * Z3.Expr.expr) list ref) (x: AST.expr): Z3.Expr.expr =
    match x with
    | Expr_Var(v) when AST.pprint_ident v = "TRUE" -> Z3.Boolean.mk_true ctx
    | Expr_Var(v) when AST.pprint_ident v = "FALSE" -> Z3.Boolean.mk_false ctx
    | Expr_Var(v) ->
        let boolsort = Z3.Boolean.mk_sort ctx in
        Z3.Expr.mk_const_s ctx (AST.pprint_ident v) boolsort

    (* Bool Ops *)
    | Expr_TApply (FIdent ("eq_bool",_), [], [a; b]) ->
        let a = z3_of_bool ctx ufs a in
        let b = z3_of_bool ctx ufs b in
        Z3.Boolean.mk_eq ctx a b
    | Expr_TApply (FIdent ("not_bool",_), [], [a]) ->
        let a = z3_of_bool ctx ufs a in
        Z3.Boolean.mk_not ctx a
    | Expr_TApply (FIdent ("and_bool",_), [], [a; b]) ->
        let a = z3_of_bool ctx ufs a in
        let b = z3_of_bool ctx ufs b in
        Z3.Boolean.mk_and ctx [a;b]
    | Expr_TApply (FIdent ("or_bool",_), [], [a; b]) ->
        let a = z3_of_bool ctx ufs a in
        let b = z3_of_bool ctx ufs b in
        Z3.Boolean.mk_or ctx [a;b]

    (* Int Ops *)
    | Expr_TApply (FIdent ("eq_int",_), [], [a; b]) ->
        let (an,ad) = extract_div a in
        let (bn,bd) = extract_div b in
        let a = z3_of_expr ctx ufs (mk_mul an bd) in
        let b = z3_of_expr ctx ufs (mk_mul bn ad) in
        Z3.Boolean.mk_eq ctx a b
    | Expr_TApply (FIdent ("lt_int",_), [], [a; b]) ->
        let a = z3_of_expr ctx ufs a in
        let b = z3_of_expr ctx ufs b in
        Z3.Arithmetic.mk_lt ctx a b
    | Expr_TApply (FIdent ("le_int",_), [], [a; b]) ->
        let a = z3_of_expr ctx ufs a in
        let b = z3_of_expr ctx ufs b in
        Z3.Arithmetic.mk_le ctx a b
    | Expr_TApply (FIdent ("gt_int",_), [], [a; b]) ->
        let a = z3_of_expr ctx ufs a in
        let b = z3_of_expr ctx ufs b in
        Z3.Arithmetic.mk_gt ctx a b
    | Expr_TApply (FIdent ("ge_int",_), [], [a; b]) ->
        let a = z3_of_expr ctx ufs a in
        let b = z3_of_expr ctx ufs b in
        Z3.Arithmetic.mk_ge ctx a b

    | _ -> uninterp ctx ufs x (Z3.Boolean.mk_sort ctx)

and z3_of_expr (ctx: Z3.context) (ufs: (AST.expr * Z3.Expr.expr) list ref) (x: AST.expr): Z3.Expr.expr =
    match x with
    | Expr_Var(v) ->
        let intsort = Z3.Arithmetic.Integer.mk_sort ctx in
        Z3.Expr.mk_const_s ctx (AST.pprint_ident v) intsort
    | Expr_LitInt i -> Z3.Arithmetic.Integer.mk_numeral_s ctx i

    | Expr_TApply (FIdent ("add_int",_),  [], xs) ->
        Z3.Arithmetic.mk_add ctx (List.map (z3_of_expr ctx ufs) xs)
    | Expr_TApply (FIdent ("sub_int",_),  [], xs) ->
        Z3.Arithmetic.mk_sub ctx (List.map (z3_of_expr ctx ufs) xs)
    | Expr_TApply (FIdent ("mul_int",_),  [], xs) ->
        Z3.Arithmetic.mk_mul ctx (List.map (z3_of_expr ctx ufs) xs)

    | Expr_TApply (FIdent ("pow2_int",_),  [], [Expr_LitInt i]) when int_of_string i > 0 ->
        let i = int_of_string i in
        Z3.Arithmetic.mk_mul ctx (List.init i (fun _ -> Z3.Arithmetic.Integer.mk_numeral_i ctx 2))
    | Expr_TApply (FIdent ("pow2_int",_),  [], [Expr_LitInt i]) when int_of_string i = 0 ->
        Z3.Arithmetic.Integer.mk_numeral_i ctx 1

    (* TODO: technically unsound, need proof that 0 <= i <= 30 *)
    | Expr_TApply (FIdent ("pow2_int",_),  [], [i]) ->
        let i = z3_of_expr ctx ufs i in
        let rec loop n =
          if n = 0 then Z3.Arithmetic.Integer.mk_numeral_i ctx 1
          else Z3.Boolean.mk_ite ctx (Z3.Boolean.mk_eq ctx (Z3.Arithmetic.Integer.mk_numeral_i ctx n) i) (Z3.Arithmetic.Integer.mk_numeral_i ctx (1 lsl n)) (loop (n - 1))
        in
        loop 30

    | Expr_If (_, c, t, [], f) ->
        let c = z3_of_bool ctx ufs c in
        let t = z3_of_expr ctx ufs t in
        let f = z3_of_expr ctx ufs f in
        Z3.Boolean.mk_ite ctx c t f

    | _ -> uninterp ctx ufs x (Z3.Arithmetic.Integer.mk_sort ctx)

(** check that bs => cs *)
let check_constraints (bs: AST.expr list) (cs: AST.expr list): bool =
    (* note that we rebuild the Z3 context each time.
     * It is possible to share them across all invocations to save
     * about 10% of execution time.
     *)
    let z3_ctx = Z3.mk_context [] in
    let solver = Z3.Solver.mk_simple_solver z3_ctx in
    let ufs = ref [] in (* uninterpreted function list *)
    let bs' = List.map (fun e -> z3_of_bool z3_ctx ufs (Asl_utils.prune_parens e)) bs in
    let cs' = List.map (fun e -> z3_of_bool z3_ctx ufs (Asl_utils.prune_parens e)) cs in
    let p = Z3.Boolean.mk_implies z3_ctx (Z3.Boolean.mk_and z3_ctx bs') (Z3.Boolean.mk_and z3_ctx cs') in
    if verbose then Printf.printf "      - Checking %s\n" (Z3.Expr.to_string p);
    Z3.Solver.add solver [Z3.Boolean.mk_not z3_ctx p];
    let q = Z3.Solver.check solver [] in
    if q = SATISFIABLE then Printf.printf "Failed property %s\n" (Z3.Expr.to_string p);
    if q = UNKNOWN then Printf.printf "Unknown property %s\n" (Z3.Expr.to_string p);
    q = UNSATISFIABLE
