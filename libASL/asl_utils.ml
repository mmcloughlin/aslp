(****************************************************************
 * ASL utility functions
 *
 * Copyright Arm Limited (c) 2017-2019
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

(** ASL utility functions *)

module PP   = Asl_parser_pp
module AST  = Asl_ast

open AST
open Asl_visitor

(****************************************************************)
(** {2 Bindings and IdentSet}                                   *)
(****************************************************************)

(** {2 Bindings: maps indexed by identifiers} *)
module Bindings = Map.Make(AST.Id)

(** add association list to bindings *)
let add_bindings (bs: 'a Bindings.t) (xs: (ident * 'a) list): 'a Bindings.t =
    List.fold_left (fun a (k, v) -> Bindings.add k v a) bs xs

(** create bindings from association list *)
let mk_bindings (xs: (ident * 'a) list): 'a Bindings.t =
    add_bindings Bindings.empty xs

(** print bindings *)
let pp_bindings (pp: 'a -> string) (bs: 'a Bindings.t): string =
    String.concat ", " (List.map (fun (k, v) -> pprint_ident k ^"->"^ pp v) (Bindings.bindings bs))

let bindings_of_list (l: (ident * 'a) list): 'a Bindings.t =
    List.fold_right (fun (k,v) -> Bindings.add k v) l Bindings.empty

(** {2 Sets of identifiers} *)
module IdentSet = Set.Make(Id)

let pp_identset is =
    String.concat ", " (List.map (fun k -> pprint_ident k) (IdentSet.elements is))

(** merge a list of sets *)
let unionSets (idss: IdentSet.t list): IdentSet.t =
    List.fold_left IdentSet.union IdentSet.empty idss

(** add v to set of identifiers mapped to k *)
let addToBindingSet (k: ident) (v: ident) (bs: IdentSet.t Bindings.t): IdentSet.t Bindings.t =
    Bindings.update k (fun old ->
        (match old with
        | None    -> Some (IdentSet.singleton v)
        | Some vs -> Some (IdentSet.add v vs)
        )
    ) bs

(** convert identifier set to sorted list of identifiers

    The implementation is trivial and exists mostly to emphasize that the
    resulting list is sorted
 *)
let to_sorted_list (s: IdentSet.t): ident list =
    IdentSet.elements s

let bindings_domain (b: 'a Bindings.t): IdentSet.t =
  Bindings.fold (fun k _ -> IdentSet.add k) b IdentSet.empty

(****************************************************************)
(** {2 Equivalence classes}                                     *)
(****************************************************************)

(** Equivalence classes are represented by trees.

    The root of the tree is the canonical member of the class.
    Traversing the parent node takes you closer to the canonical member.
    The root is its own parent.
 *)
type tree = {
    mutable parent : tree;
    data : ident;
}

(** Equivalence class support (to support unification, and similar)

    The implementation is based on
    {{:https://en.wikipedia.org/wiki/Disjoint-set_data_structure}Wikipedia: Union-Find}.
    I have not implemented all the optimizations they suggest
    because I expect sets to be quite small in practice.
 *)

class equivalences = object (self)

    (* Mapping from elements to the set containing them *)
    val mutable forest : tree Bindings.t = Bindings.empty

    (* Find the root (canonical member of) the set.
     * Implements "path-splitting" optimisation that makes every node
     * point to its grandfather so each traversal reduces height of tree.
     *)
    method private find (x: tree): tree =
        let r = ref x in
        while !r.parent != !r do
            let next = !r.parent in
            !r.parent <- next.parent;
            r := next
        done;
        !r

    (* Find the root of the set containing 'x' - creating a new
     * set if not already known *)
    method private find_ident (x: ident): tree =
        let s = (match Bindings.find_opt x forest with
        | None ->
                let rec t = { parent = t; data = x; } in
                t
        | Some t ->
                self#find t
        ) in
        forest <- Bindings.add x s forest;
        s

    (* Find the canonical member of the set containing 'x' *)
    method canonicalize (x: ident): ident =
        let s = self#find_ident x in
        s.data

    (* Merge the sets containing 'x' and 'y' *)
    method merge (x: ident) (y: ident): unit =
        let x' = self#find_ident x in
        let y' = self#find_ident y in
        if x != y then y'.parent <- x'

    (* Optimization: short circuit every tree so that they all point directly at root *)
    method private normalize: unit =
        forest <- Bindings.map (self#find) forest

    (* Return mapping from identifiers to the canonical representation of their
     * equivalence class
     *)
    method mapping: ident Bindings.t =
        self#normalize;
        Bindings.map (fun t -> (self#find t).data) forest

    (* Construct equivalence classes for each canonical member of a class.
     *
     * The implementation of this could be made more efficient by adding
     * pointers to trees so that we can map each canonical member to a
     * tree containing all the nodes that point to it.
     * But this implementation just does a linear scan over all the members
     * of the forest.
     *)
    method classes: IdentSet.t Bindings.t =
        Bindings.fold (fun k v -> addToBindingSet v k) self#mapping Bindings.empty

    (* Print equivalence classes adding a prefix at the start of every line of
     * output.
     *)
    method pp (prefix: string): unit =
        Bindings.iter (fun v vs ->
            Printf.printf "%s%s -> {" prefix (pprint_ident v);
            IdentSet.iter (fun w -> Printf.printf " %s" (pprint_ident w)) vs;
            Printf.printf "}\n";
        ) self#classes
end


(****************************************************************)
(** {1 AST Transformation Utilities}                            *)
(****************************************************************)

(****************************************************************)
(** {2 Calculating free variables of expressions and types}     *)
(****************************************************************)

class freevarClass = object
    inherit nopAslVisitor

    val mutable fvs = IdentSet.empty
    method result = fvs
    method! vvar x =
        fvs <- IdentSet.add x fvs;
        SkipChildren
    method! vtype ty =
        match ty with
        | Type_Register _ ->
           (* Free variables in register types are not supported and will
              lead to a type error.

              Uses of global constants and variables in the indices of field
              declarations of a register type are allowed, though, and will
              be checked by the type checker as usual.  Note that they will
              not be evaluated at register declaration time, but every time
              the respective register field is accessed (the type checker
              desugars register field accesses to slice expressions, copying
              the field indices). *)
           SkipChildren
        | _ -> DoChildren
end

let fv_expr (x: expr): IdentSet.t =
    let fv = new freevarClass in
    ignore (visit_expr (fv :> aslVisitor) x);
    fv#result

let fv_type (x: ty): IdentSet.t =
    let fv = new freevarClass in
    ignore (visit_type (fv :> aslVisitor) x);
    fv#result

let fv_args (atys: (ty * ident) list): IdentSet.t =
    unionSets (List.map (fun (ty, _) -> fv_type ty) atys)

let fv_sformal (x: sformal): IdentSet.t =
    (match x with
    | Formal_In(ty,v) -> fv_type ty
    | Formal_InOut(ty,v) -> fv_type ty
    )

let fv_sformals (atys: sformal list): IdentSet.t =
    unionSets (List.map fv_sformal atys)

let fv_stmts stmts =
    let fvs = new freevarClass in
    ignore (visit_stmts (fvs :> aslVisitor) stmts);
    fvs#result

let fv_stmt stmt =
    let fvs = new freevarClass in
    ignore (visit_stmt_single (fvs :> aslVisitor) stmt);
    fvs#result

let fv_decl decl =
    let fvs = new freevarClass in
    ignore (visit_decl (fvs :> aslVisitor) decl);
    fvs#result

(****************************************************************)
(** {2 Calculating assigned variables in statements}            *)
(****************************************************************)

class assignedVarsClass = object
    inherit nopAslVisitor

    val mutable avs = IdentSet.empty
    method result = avs
    method! vlvar x =
        avs <- IdentSet.add x avs;
        SkipChildren
end

let assigned_vars_of_stmts stmts =
    let avs = new assignedVarsClass in
    ignore (visit_stmts (avs :> aslVisitor) stmts);
    avs#result

let assigned_vars_of_decl decl =
    let avs = new assignedVarsClass in
    ignore (visit_decl (avs :> aslVisitor) decl);
    avs#result

(****************************************************************)
(** {2 Collect local bindings (variables and constants)}        *)
(****************************************************************)

class localsClass = object (self)
    inherit nopAslVisitor

    val mutable stack = [(Bindings.empty : ty Bindings.t)]
    method locals =
        let merge _ x y = Some x in
        List.fold_right (Bindings.union merge) stack Bindings.empty

    method add_local (ty, id) =
        match stack with
        | s :: ss -> stack <- (Bindings.add id ty s :: ss)
        | [] -> failwith "addLocal: empty stack"
    method! enter_scope vars =
        stack <- Bindings.empty :: stack;
        List.iter self#add_local vars
    method! leave_scope () =
        match stack with
        | s :: ss -> stack <- ss
        | [] -> failwith "leave_scope: empty stack"
    method! vstmt = function
        | Stmt_VarDecl (ty, id, _, _)
        | Stmt_ConstDecl (ty, id, _, _) ->
            self#add_local (ty, id);
            DoChildren
        | Stmt_VarDeclsNoInit (ty, ids, _) ->
            List.iter (fun id -> self#add_local (ty, id)) ids;
            DoChildren
        | _ ->
            DoChildren
end

let locals_of_stmts stmts =
    let lc = new localsClass in
    ignore @@ Asl_visitor.visit_stmts lc stmts;
    lc#locals

let locals_of_decl decl =
    let lc = new localsClass in
    ignore (Visitor.mapNoCopy (visit_decl (lc :> aslVisitor)) decl);
    lc#locals

(****************************************************************)
(** {2 Calculate types used in expressions and statements}      *)
(****************************************************************)

class typesClass = object
  inherit nopAslVisitor

  val mutable types = IdentSet.empty
  method result = types
  method! vtype ty =
    match ty with
    | Type_Constructor id
    | Type_App (id, _) ->
       types <- IdentSet.add id types;
       DoChildren
    | _ ->
       DoChildren
end

let types_of_expr expr =
  let cc = new typesClass in
  ignore (visit_expr (cc :> aslVisitor) expr);
  cc#result

let types_of_stmts stmts =
  let cc = new typesClass in
  ignore (visit_stmts (cc :> aslVisitor) stmts);
  cc#result

let types_of_decl decl =
  let cc = new typesClass in
  ignore (visit_decl (cc :> aslVisitor) decl);
  cc#result

(****************************************************************)
(** {2 Calculate functions and procedures called in statements} *)
(****************************************************************)

class callsClass = object
  inherit nopAslVisitor

  val mutable calls = IdentSet.empty
  method result = calls
  method! vexpr = function
    | Expr_TApply (f, _, _) ->
       calls <- IdentSet.add f calls;
       DoChildren
    | _ -> DoChildren
  method! vstmt = function
    | Stmt_TCall (id, _, _, _) ->
       calls <- IdentSet.add id calls;
       DoChildren
    | _ -> DoChildren
  method! vlexpr = function
    | LExpr_Write (id, _, _) ->
       calls <- IdentSet.add id calls;
       DoChildren
    | LExpr_ReadWrite (id1, id2, _, _) ->
       calls <- IdentSet.add id1 calls |> IdentSet.add id2;
       DoChildren
    | _ -> DoChildren
end

let calls_of_expr expr =
  let cc = new callsClass in
  ignore (visit_expr (cc :> aslVisitor) expr);
  cc#result

let calls_of_stmts stmts =
  let cc = new callsClass in
  ignore (visit_stmts (cc :> aslVisitor) stmts);
  cc#result

let calls_of_decl decl =
  let cc = new callsClass in
  ignore (visit_decl (cc :> aslVisitor) decl);
  cc#result

(****************************************************************)
(** {2 Substitutions}                                           *)
(****************************************************************)

(** Performing variable substitutions in expressions and types

    Note that it does not replace type constructors, global constants
    or enumerations in patterns, array indexes and types so this is
    limited to replacing local variables.
    It also does not replace variables used as l-expressions though
    that it easily changed if we think it should.               *)
class substClass (s: expr Bindings.t) = object
    inherit nopAslVisitor
    method! vexpr x =
        (match x with
        | Expr_Var v ->
                (match Bindings.find_opt v s with
                | Some r -> ChangeTo r
                | None -> DoChildren
                )
        | _ -> DoChildren
        )
end

let subst_expr (s: expr Bindings.t) (x: expr): expr =
    let subst = new substClass s in
    visit_expr subst x

let subst_lexpr (s: expr Bindings.t) (x: lexpr): lexpr =
    let subst = new substClass s in
    visit_lexpr subst x

let subst_slice (s: expr Bindings.t) (x: slice): slice =
    let subst = new substClass s in
    visit_slice subst x

let subst_type (s: expr Bindings.t) (x: ty): ty =
    let subst = new substClass s in
    visit_type subst x

let subst_stmt (s: expr Bindings.t) (x: stmt): stmt =
    let subst = new substClass s in
    visit_stmt_single subst x


(** More flexible substitution class - takes a function instead
    of a binding set.
 *)
class substFunClass (replace: ident -> expr option) = object
    inherit nopAslVisitor
    method! vexpr x =
        (match x with
        | Expr_Var v ->
                (match replace v with
                | Some r -> ChangeTo r
                | None -> DoChildren
                )
        | _ -> DoChildren
        )
end

let subst_fun_expr (replace: ident -> expr option) (x: expr): expr =
    let subst = new substFunClass replace in
    visit_expr subst x

let subst_fun_lexpr (replace: ident -> expr option) (x: lexpr): lexpr =
    let subst = new substFunClass replace in
    visit_lexpr subst x

let subst_fun_slice (replace: ident -> expr option) (x: slice): slice =
    let subst = new substFunClass replace in
    visit_slice subst x

let subst_fun_type (replace: ident -> expr option) (x: ty): ty =
    let subst = new substFunClass replace in
    visit_type subst x

(****************************************************************)
(** {2 Expression transformation}                               *)
(****************************************************************)

(** Expression transformation class

    Applies replace function to any subexpression.
    (Especially useful for expressions in types)                *)
class replaceExprClass (replace: expr -> expr option) = object
    inherit nopAslVisitor
    method! vexpr x =
        (match replace x with
        | Some r -> ChangeTo r
        | None -> SkipChildren
        )
end

(****************************************************************)
(** {2 Resugaring}                                              *)
(****************************************************************)

(** Resugaring transform

    The typechecker desugars infix syntax to make it absolutely explicit
    what it means.  This is good for tools but bad for humans.

    This transformation re-introduces the infix syntax - the intention
    being that you might use this in error messages.
    It also deletes type parameters - so this is (more or less)
    the reverse of typechecking.                                *)
class resugarClass (ops: AST.binop Bindings.t) = object (self)
    inherit nopAslVisitor
    method! vexpr x =
        (match x with
        | Expr_TApply(f, tys, args) ->
                let args' = List.map (visit_expr (self :> aslVisitor)) args in
                (match (Bindings.find_opt f ops, args') with
                | (Some op, [a; b]) -> ChangeTo (Expr_Parens(Expr_Binop(a, op, b)))
                (* | (Some op, [a]) -> ChangeTo (Expr_Unop(op, a)) *)
                | _ -> ChangeTo (Expr_TApply(f, [], args'))
                )
        | _ ->
                DoChildren
        )
end

let resugar_expr (ops: AST.binop Bindings.t) (x: expr): expr =
    let resugar = new resugarClass ops in
    visit_expr resugar x

let resugar_type (ops: AST.binop Bindings.t) (x: AST.ty): AST.ty =
    let resugar = new resugarClass ops in
    visit_type resugar x

(****************************************************************)
(** {2 Pretty printing wrappers}                                *)
(****************************************************************)

let pp_type  (x: ty):    string = Utils.to_string (PP.pp_ty    x)
let pp_expr  (x: expr):  string = Utils.to_string (PP.pp_expr  x)
let pp_lexpr (x: lexpr): string = Utils.to_string (PP.pp_lexpr x)
let pp_stmt  (x: stmt):  string = Utils.to_string (PP.pp_stmt  x)

let pp_decode_pattern (x: decode_pattern) = Utils.to_string (PP.pp_decode_pattern x)

let pp_decode_slice (x: decode_slice) = Utils.to_string (PP.pp_decode_slice x)

let pp_decode_alt (DecoderAlt_Alt(ps, _): decode_alt) = "when (" ^ String.concat ", " (List.map pp_decode_pattern ps) ^ ")"
let pp_decode_case (DecoderCase_Case(slices,_,_): decode_case) = "case (" ^ String.concat ", " (List.map pp_decode_slice slices) ^ ")"

let pp_instr_field (IField_Field(name,_,_)) = pprint_ident name


(****************************************************************)
(** {2 Misc}                                                    *)
(****************************************************************)

(** Length of bitstring or mask literal.

    ASL bit and mask literals allow spaces to be included - these
    do not count towards the length of the literal.
 *)
let masklength (x: string): int =
    let r = ref 0 in
    String.iter (function ' ' -> () | _ -> r := !r + 1) x;
    !r

(* Location of a statement *)
let get_loc s =
  match s with
  | Stmt_If(_, _, _, _, loc)
  | Stmt_VarDeclsNoInit(_, _, loc)
  | Stmt_VarDecl(_, _, _, loc)
  | Stmt_ConstDecl(_, _, _, loc)
  | Stmt_Assign(_,_,loc)
  | Stmt_FunReturn(_,loc)
  | Stmt_ProcReturn(loc)
  | Stmt_Assert(_, loc)
  | Stmt_Unpred loc
  | Stmt_ConstrainedUnpred loc
  | Stmt_ImpDef (_, loc)
  | Stmt_Undefined loc
  | Stmt_ExceptionTaken loc
  | Stmt_Dep_Unpred loc
  | Stmt_Dep_Undefined loc
  | Stmt_See (_,loc)
  | Stmt_Throw (_, loc)
  | Stmt_DecodeExecute (_, _, loc)
  | Stmt_TCall (_, _, _, loc)
  | Stmt_Case (_, _, _, loc)
  | Stmt_For (_, _, _, _, _, loc)
  | Stmt_While (_, _, loc)
  | Stmt_Repeat (_, _, loc)
  | Stmt_Try (_, _, _, _, loc)
  | Stmt_Dep_ImpDef (_, loc) -> loc

(****************************************************************)
(** {2 Function signature accessors}                            *)
(****************************************************************)

let fnsig_get_rt         (a,b,c,d,e,f) = a
let fnsig_get_typed_args (a,b,c,d,e,f) = b
let fnsig_get_targs      (a,b,c,d,e,f) = c
let fnsig_get_args       (a,b,c,d,e,f) = d
let fnsig_get_body       (a,b,c,d,e,f) = f

let fnsig_set_rt         (_,b,c,d,e,f) a = (a,b,c,d,e,f)
let fnsig_set_typed_args (a,_,c,d,e,f) b = (a,b,c,d,e,f)
let fnsig_set_targs      (a,b,_,d,e,f) c = (a,b,c,d,e,f)
let fnsig_set_args       (a,b,c,_,e,f) d = (a,b,c,d,e,f)
let fnsig_set_body       (a,b,c,d,e,_) f = (a,b,c,d,e,f)

let fnsig_upd_rt         upd (a,b,c,d,e,f) = (upd a,b,c,d,e,f)
let fnsig_upd_typed_args upd (a,b,c,d,e,f) = (a,upd b,c,d,e,f)
let fnsig_upd_targs      upd (a,b,c,d,e,f) = (a,b,upd c,d,e,f)
let fnsig_upd_args       upd (a,b,c,d,e,f) = (a,b,c,upd d,e,f)
let fnsig_upd_body       upd (a,b,c,d,e,f) = (a,b,c,d,e,upd f)

(****************************************************************
 * End
 ****************************************************************)
