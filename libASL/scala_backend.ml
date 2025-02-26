
open Visitor

open Asl_utils

open AST
open Asl_visitor
open Value

(* For splitting up functions we use type to indicate which parameters are passed by reference. *)

module StringSet = Set.Make(String)

  let globs = StringSet.of_list [
    "v_PSTATE_UAO";
    "v_PSTATE_PAN";
    "v_PSTATE_DIT";
    "v_PSTATE_SSBS";
    "v_PSTATE_G";
    "v_PSTATE_A";
    "v_PSTATE_I";
    "v_PSTATE_F";
    "v_PSTATE_D";
    "v_PSTATE_C";
    "v_PSTATE_Z";
    "v_PSTATE_V";
    "v_PSTATE_N";
    "v__PC";
    "v__R";
    "v__Z";
    "v_SP_EL0";
    "v_FPSR";
    "v_FPCR";
    "v_PSTATE_BTYPE";
    "v_BTypeCompatible";
    "v___BranchTaken";
    "v_BTypeNext";
    "v___ExclusiveLocal"
  ]

  let prims = StringSet.of_list (["mkBits"; "bvextract"; "f_eq_bits"; "f_ne_bits"; "f_add_bits"; "f_sub_bits";
        "f_mul_bits"; "f_and_bits"; "f_or_bits"; "f_eor_bits"; "f_not_bits";
        "f_slt_bits"; "f_sle_bits"; "f_zeros_bits"; "f_ones_bits";
        "f_ZeroExtend"; "f_SignExtend"; "f_asr_bits"; "f_lsl_bits";
        "f_lsr_bits"; "f_decl_bool"; "f_decl_bv"; "f_AtomicEnd";
        "f_AtomicStart"; "f_replicate_bits"; "f_append_bits"; "f_gen_BFAdd";
        "f_gen_BFMul"; "f_gen_FPAdd"; "f_gen_FPCompare"; "f_gen_FPCompareEQ";
        "f_gen_FPCompareGE"; "f_gen_FPCompareGT"; "f_gen_FPConvert";
        "f_gen_FPConvertBF"; "f_gen_FPDiv"; "f_gen_FPMax"; "f_gen_FPMaxNum";
        "f_gen_FPMin"; "f_gen_FPMinNum"; "f_gen_FPMul"; "f_gen_FPMulAdd";
        "f_gen_FPMulAddH"; "f_gen_FPMulX"; "f_gen_FPRSqrtStepFused";
        "f_gen_FPRecipEstimate"; "f_gen_FPRecipStepFused"; "f_gen_FPRecpX";
        "f_gen_FPRoundInt"; "f_gen_FPRoundIntN"; "f_gen_FPSqrt"; "f_gen_FPSub";
        "f_gen_FPToFixed"; "f_gen_FPToFixedJS_impl"; "f_gen_FixedToFP";
        "f_gen_bit_lit"; "f_gen_bool_lit"; "f_gen_branch"; "f_cvt_bits_uint";
        "f_gen_cvt_bits_uint"; "f_gen_cvt_bool_bv"; "f_gen_eor_bits";
        "f_gen_eq_bits"; "f_gen_eq_enum"; "f_gen_int_lit"; "f_gen_store";
        "f_gen_load"; "f_gen_SignExtend"; "f_gen_ZeroExtend"; "f_gen_add_bits";
        "f_gen_and_bits"; "f_gen_and_bool"; "f_gen_asr_bits"; "f_gen_lsl_bits";
        "f_gen_lsr_bits"; "f_gen_mul_bits"; "f_gen_ne_bits"; "f_gen_not_bits";
        "f_gen_not_bool"; "f_gen_or_bits"; "f_gen_or_bool"; "f_gen_sdiv_bits";
        "f_gen_sle_bits"; "f_gen_slt_bits"; "f_gen_sub_bits";
        "f_gen_AArch64_MemTag_set"; "f_gen_Mem_read"; "f_gen_slice";
        "f_gen_replicate_bits"; "f_gen_append_bits"; "f_gen_array_load";
        "f_gen_array_store"; "f_gen_Mem_set"; "f_gen_assert";
        "f_switch_context"; "f_true_branch"; "f_false_branch"; "f_merge_branch"])

let mutable_decl = "class Mutable[T](var v: T)"

type var_type = 
  | Mutable of ty
  | Immutable of ty
  | Unit
  | Infer (* Omit the type def on scala side *)

type sc_fun_sig = {
  rt: var_type ;
  arg_types: (var_type * ident) list;
  targs: ident list;
  args: ident list;
  body: stmt list;
}

module DefSet = Set.Make(struct 
  type t = (ident * ty * bool) 
  let compare (a,b,e) (c,d,f) = (match (Stdlib.compare a c) with
      | 0 -> (match (Stdlib.compare b d ) with 
        | 0 -> (Stdlib.compare e f )
        | s -> s)
      | s -> s)
end)

let compose a b f = b (a f) 

let (let@) x f = fun s ->
  let (s,r) = x s in
  (f r) s
let (let+) x f = fun s ->
  let (s,r) = x s in
  (s,f r)


module LocMap = Map.Make(struct
  type t = l
  let compare = Stdlib.compare
end)

class find_defs = object (self)
  inherit Asl_visitor.nopAslVisitor
  val mutable defs = LocMap.empty 

  method add_dep loc i = defs <- LocMap.add loc i defs 
  method get_deps = defs

  method! vstmt = function
    | Stmt_VarDeclsNoInit(ty, [v], loc) -> self#add_dep loc (v, ty); SkipChildren
    | Stmt_VarDecl(ty, v, e, loc) -> self#add_dep loc (v ,ty); SkipChildren
    | Stmt_ConstDecl(ty, v, e, loc) -> self#add_dep loc (v , ty) ; SkipChildren
    | _ -> DoChildren
end 


type stvarinfo = {
  ident : ident;
  var : expr;
  typ : var_type; 
}

let state_var = { var = Expr_Var (Ident "st"); typ = Immutable (Type_Constructor (Ident "LiftState[RTSym, RTLabel, BV]")); ident = Ident "st" } 

type st = {
  mutable indent: int;
  mutable skip_seq: bool;
  oc : out_channel;

  (* variables that are access thru mutable.v field *)
  mutable mutable_vars : var_type Transforms.ScopedBindings.t;

  (* New functions generated by splitting functions *)
  mutable extra_functions : sc_fun_sig Bindings.t;
}

let define (st) (t: var_type) (v:ident) = Stack.push (Stack.pop st.mutable_vars |> Bindings.add v t) st.mutable_vars
let push_scope st = Stack.push  Bindings.empty (st.mutable_vars)
let pop_scope st = Stack.pop (st.mutable_vars) |> ignore

let var_mutable (v:ident) (st) = 
  let find_def = Transforms.ScopedBindings.find_binding in
  match (find_def st.mutable_vars v) with 
    | Some Mutable _ -> true
    | Some Unit -> false
    | Some Infer -> true
    | Some Immutable _ -> false
  | None ->  true (* Globals are mutable by default *)

let global_imports = []
let global_opens = []

let uniq_counter : int ref = ref 0 

let new_index _ : int = uniq_counter := !uniq_counter + 1 ; !uniq_counter
let new_name pref = Ident ( pref ^ "_" ^ (string_of_int (new_index ())))
let new_indexs (b: string) : string = b ^ (string_of_int (new_index ()))
  
class stmt_counter = object(this)
  inherit Asl_visitor.nopAslVisitor
  val mutable stmt_count: int  = 0
  val mutable expr_count: int  = 0

  method !vstmt s = stmt_count <- stmt_count + 1; DoChildren

  method !vexpr s = expr_count <- expr_count + 1; DoChildren
  
  method count (s:stmt) : int = stmt_count <- 0; (visit_stmt this s) |> ignore; stmt_count

  method expr_count (e:expr) : int = expr_count <- 0; (visit_expr this e) |> ignore ; expr_count
  method gexpr_count = expr_count
end

let sl_complexity(sl:stmt list) : int = let s = new stmt_counter in visit_stmts s (sl) |> ignore ; s#gexpr_count  
let count_stmts_list (s:stmt list) : int list = List.map ((new stmt_counter)#count) s
let count_stmts (s:stmt) : int = (new stmt_counter)#count s

(* Shallow inspection of an expression to guess its type. *)
let infer_type e : ty option =
    let tint = Some (Type_Constructor (Ident "integer")) in
    let tbool  = Some (Type_Constructor (Ident "boolean"))  in
    let tbits = fun b -> Some (Type_Bits b) in
  match e with
  (* Boolean Expressions *)
  | Expr_Var(Ident "TRUE") -> tbool
  | Expr_Var(Ident "FALSE") -> tbool
  | Expr_TApply(FIdent("and_bool", 0), [], [a;b]) -> tbool
  | Expr_TApply(FIdent("or_bool", 0), [], [a;b]) -> tbool
  | Expr_TApply(FIdent("implies_bool", 0), [], [a;b]) -> tbool
  | Expr_TApply(FIdent("not_bool", 0), [], [a]) -> tbool

  (* Int Expressions using Z *)
  | Expr_LitInt i -> tint
  | Expr_TApply(FIdent("add_int", 0), [], [a;b]) -> tint
  | Expr_TApply(FIdent("sub_int", 0), [], [a;b]) -> tint
  | Expr_TApply(FIdent("mul_int", 0), [], [a;b]) -> tint
  | Expr_TApply(FIdent("frem_int", 0), [], [a;b]) -> tint

  (* Other operations *)
  | Expr_LitBits b -> tbits (Expr_LitInt (b))
  | Expr_Slices(e,[Slice_LoWd(i,w)]) -> tbits (w)
  | _ -> None


let prints_arg_type (t: var_type) : string =
  let rec ctype t = 
  match t with
    | (Type_Bits _) -> "BV" 
    | (Type_Constructor (Ident "integer")) -> "BigInt"
    | (Type_Constructor (Ident "boolean")) -> "Boolean"
    | (Type_Tuple l) -> ": (" ^ (String.concat "," (List.map ctype (l)) ) ^ ")"
    | Type_Constructor (Ident "rt_label") -> "RTLabel"
    | Type_Constructor (Ident "rt_sym") -> "RTSym" 
  | Type_Constructor (Ident "rt_expr") -> "RTSym"
    | Type_Constructor (Ident e) -> e 
    | t -> failwith @@ "Unknown arg type: " ^ (pp_type t)
  in
  match t with 
      | Mutable v -> Printf.sprintf "Mutable[%s]" (ctype v)
      | Immutable v -> ctype v
      | Infer -> ""
      | Unit -> "Unit"


(****************************************************************
 * String Utils
 ****************************************************************)

let inc_depth st = st.indent <- st.indent + 2
let dec_depth st = st.indent <- st.indent - 2

let replace s =
  String.fold_left (fun acc c ->
    if c = '.' then acc ^ "_"
    else if c = '#' then acc ^ "HASH"
    else acc ^ (String.make 1 c)) "" s

let plain_ident v : string =
  let s = (match v with
  | Ident n ->  n
  | FIdent (n,0) ->  n
  | FIdent (n,i) ->  n ^ "_" ^ (string_of_int i)) in
  replace s

let name_of_ident v : string =
  let s = (match v with
  | Ident n -> "v_" ^ n
  | FIdent (n,0) -> "f_" ^ n
  | FIdent (n,i) -> "f_" ^ n ^ "_" ^ (string_of_int i)) in
  replace s

let rec name_of_lexpr l =
  match l with
  | LExpr_Var v -> name_of_ident v
  | LExpr_Field (l, f) ->
      let l = name_of_lexpr l in
      let f = name_of_ident f in
      l ^ "." ^ f
  | LExpr_Wildcard -> "_"
  | _ -> failwith @@ "name_of_lexpr: " ^ (pp_lexpr l)

(* Expr printing *)


let rec prints_expr ?(deref:bool=true) e (st:st)  =
  match e with
  (* Boolean Expressions *)
  | Expr_Var(Ident "TRUE") -> "true"
  | Expr_Var(Ident "FALSE") -> "false"
  | Expr_TApply(FIdent("and_bool", 0), [], [a;b]) ->
      Printf.sprintf "((%s) && (%s))" (prints_expr a st ~deref) (prints_expr b st ~deref)
  | Expr_TApply(FIdent("or_bool", 0), [], [a;b]) ->
      Printf.sprintf "((%s) || (%s))" (prints_expr a st ~deref) (prints_expr b st ~deref)
  | Expr_TApply(FIdent("implies_bool", 0), [], [a;b]) ->
      Printf.sprintf "((!(%s)) || (%s))" (prints_expr a st ~deref) (prints_expr b st ~deref)
  | Expr_TApply(FIdent("not_bool", 0), [], [a]) ->
      Printf.sprintf " (!(%s))" (prints_expr a st ~deref)

  (* State Accesses *)
  | Expr_Var(v) -> 
    let name = (name_of_ident v) in
    let name = if (StringSet.mem name globs) then ("v_st." ^ name) else name in
    if (deref && (var_mutable v st)) then (name ^ ".v" ) else name
  | Expr_Field(e, f) ->
      prints_expr e st ^ "." ^ name_of_ident f
  | Expr_Array(a,i) ->
      Printf.sprintf "(%s).get(%s)" (prints_expr a st) (prints_expr i st)

  (* Int Expressions using Z *)
  | Expr_LitInt i -> "BigInt(" ^ i ^ ")"
  | Expr_TApply(FIdent("add_int", 0), [], [a;b]) ->
      Printf.sprintf "((%s) + (%s))" (prints_expr a st) (prints_expr b st)
  | Expr_TApply(FIdent("sub_int", 0), [], [a;b]) ->
      Printf.sprintf "((%s) - (%s))" (prints_expr a st) (prints_expr b st)
  | Expr_TApply(FIdent("mul_int", 0), [], [a;b]) ->
      Printf.sprintf "((%s) * (%s))" (prints_expr a st) (prints_expr b st)
  | Expr_TApply(FIdent("frem_int", 0), [], [a;b]) ->
      let x  = (prints_expr a st) in let y = (prints_expr b st)  in
      Printf.sprintf "((%s) - ( (%s) * ((%s) / (%s))))"  x y x y 

  (* Other operations *)
  | Expr_TApply(FIdent("as_ref", 0), [], [a]) -> Printf.sprintf "%s" (prints_expr a st ~deref:false) 
    (*as_ref is only output by backend to idicate not to deref pointers*)
  | Expr_LitBits b -> Printf.sprintf "%s.mkBits(%d, BigInt(\"%s\", 2))" (prints_expr state_var.var st) (String.length b) b 
  | Expr_Slices(e,[Slice_LoWd(i,w)]) ->
      let e = prints_expr e st in
      let i = prints_expr i st in
      let w = prints_expr w st in
      let stv = prints_expr state_var.var st in
      Printf.sprintf "%s.bvextract(%s,%s,%s)" stv e i w
  | Expr_TApply(f, targs, args) ->
      let stv = prints_expr state_var.var st in
      let deref = not (Bindings.mem f st.extra_functions) in
      let f = (name_of_ident f) in
      let prim = (StringSet.mem f prims) in
      let args = if prim  then (targs@args) else (state_var.var::targs@args) in
      let f = if prim then (stv ^ "." ^  f) else f in
      let args = List.map (fun e -> prints_expr ~deref:deref e st) (args) in
      f ^ "(" ^ (String.concat ", " (args)) ^ ")"

  | Expr_LitString s -> "\"" ^ s ^ "\""
  | Expr_Tuple(es) -> "(" ^ (String.concat "," (List.map (fun e -> prints_expr e st) es)) ^ ")"
  | Expr_Unknown(ty) -> default_value ty st (* Sound? *)
  | Expr_If(_, c, t, [], e) -> Printf.sprintf "(if (%s) then (%s) else (%s))" (prints_expr c st) (prints_expr t st) (prints_expr e st)

  | _ -> failwith @@ "prints_expr: " ^ pp_expr e

and default_value t st =
  let stv = prints_expr state_var.var st in
  match t with
  | Type_Bits w -> Printf.sprintf "%s.mkBits(%s, BigInt(0))" stv (prints_expr w st)
  | Type_Constructor (Ident "boolean") -> "true"
  | Type_Constructor (Ident "integer") -> "BigInt(0)"
  | Type_Constructor (Ident "rt_label") -> stv ^ ".rTLabelDefault"
  | Type_Constructor (Ident "rt_sym") -> stv ^ ".rTSymDefault"
  | Type_Constructor (Ident "rt_expr") -> stv ^ ".rTExprDefault"
  | Type_Constructor (Ident "Unit") -> "()"
  | Type_Constructor (Ident "Any") -> "null"
  | Type_Array(Index_Range(lo, hi),ty) ->
      let lo = prints_expr lo st in
      let hi = prints_expr hi st in
      let d = default_value ty st in
      Printf.sprintf "Range.Exclusive((%s), (%s)).map(%s).toList" lo hi d
  | _ -> failwith @@ "Unknown type for default value: " ^ (pp_type t)



(* End expr printing *)


let write_line s st =
  let padding = String.concat "" (List.init st.indent (fun _ -> " ")) in
  Printf.fprintf st.oc  "%s%s" padding s

let write_seq st =
  if st.skip_seq then
    st.skip_seq <- false
  else Printf.fprintf st.oc "\n"

let write_nl st =
  Printf.fprintf st.oc "\n"

(****************************************************************
 * Prim Printing
 ****************************************************************)

let write_fun_return e st =
  let s = Printf.sprintf "%s" e in
  write_line s st

let write_proc_return st =
  write_line "/*proc return */ ()" st

let write_assert s st =
  let s = Printf.sprintf "assert (%s)" s in
  write_line s st

let write_unsupported st =
  write_line "throw Exception(\"not supported\")" st

let write_call f (targs : typeid list) (args: typeid list) st =
  let stv = prints_expr state_var.var st in
  let prim = (StringSet.mem f prims) in
  let f = if prim then (stv ^ "." ^  f) else f in
  let args = if prim then (targs@args) else ((prints_expr state_var.var st)::targs@args)in
  let call = f ^ " (" ^ (String.concat "," args) ^ ")" in
  write_line call st

let write_ref v ty e st =
  let name = name_of_ident v in
  let s = Printf.sprintf "val %s = %s(%s)\n" name (prints_arg_type ty) e in
  st.skip_seq <- true;
  write_line s st

let write_let v ty e st =
  let v = name_of_ident v in
  let s = Printf.sprintf "val %s : %s = %s \n" v (prints_arg_type ty) e in
  st.skip_seq <- true;
  write_line s st

let write_if_start c st =
  let s = Printf.sprintf "if (%s) then {\n" c in
  write_line s st

let write_if_elsif c st =
  write_nl st;
  let s = Printf.sprintf "} else if (%s) then {\n" c in
  write_line s st

let write_if_else st =
  write_nl st;
  write_line "} else {\n" st

let write_if_end st =
  write_nl st;
  write_line "}" st

(****************************************************************
 * Stmt Printing
 ****************************************************************)


let prints_lexpr v st = 
  match v with
  | LExpr_Wildcard -> "_"
  | LExpr_Var v -> name_of_ident v
  | LExpr_Array (LExpr_Var v, i) -> name_of_ident v
  | LExpr_Field (l, f) -> name_of_lexpr l 
  | LExpr_Tuple (ls) -> "(" ^ String.concat "," (List.map name_of_lexpr ls) ^ ")" 
  | _ -> failwith @@ "pritns_lexpr: " ^ (pp_lexpr v)

let rec expr_of_lexpr v = 
  match v with
  | LExpr_Var v -> Expr_Var v
  | LExpr_Array (LExpr_Var v, i) -> Expr_Array (Expr_Var v, i)
  | LExpr_Field (l, f) -> Expr_Field (expr_of_lexpr l , f) 
  | LExpr_Tuple (ls) -> Expr_Tuple (List.map expr_of_lexpr ls) 
  | _ -> failwith @@ "expr_of_lexpr: " ^ (pp_lexpr v)

let rec write_assign v e st =
  match v with
  | LExpr_Wildcard ->
      failwith @@ "write_assign: " ^ (pp_lexpr v);
      (*let s = Printf.sprintf "val _ = %s \n" e in
      st.skip_seq <- true;
      write_line s st*)

  | LExpr_Var v ->
      let v =  (if (var_mutable v st) then ((name_of_ident v) ^ ".v" ) else (name_of_ident v)) in
      let s = Printf.sprintf "%s = %s" v e in
      write_line s st

  | LExpr_Array (LExpr_Var v, i) ->
      let i = prints_expr i st in
      let v = name_of_ident v in
      let s = Printf.sprintf "%s = list_update (%s, %s, %s)" v v i e in
      write_line s st

  | LExpr_Field (l, f) ->
      let v = name_of_lexpr l in
      let s = Printf.sprintf "%s = %s" v e in
      write_line s st

  | LExpr_Tuple (ls) ->
      let vars = List.init (List.length ls) (fun i -> "tmp" ^ (string_of_int (new_index ()))) in
      let v = "(" ^ String.concat "," vars ^ ")" in
      let s = Printf.sprintf "val %s = %s \n" v e in
      st.skip_seq <- true;
      write_line s st;
      List.iter2 (fun l e ->
        write_seq st;
        write_assign l e st
      ) ls vars

  | _ -> failwith @@ "write_assign: " ^ (pp_lexpr v)


module FunctionSplitter = struct
  open Transforms.ScopedBindings

  module StmtSet = Set.Make(struct 
    type t = stmt
    let compare = Stdlib.compare
  end)

  type split_ctx = {
    (* return type of containing function *)
    return_type: var_type;
    (* to look up the type of parameters *)
    bindings: var_type Bindings.t ; 
  }

  class find_returns = object(this)
    inherit Asl_visitor.nopAslVisitor
    val mutable returns = StmtSet.empty
    val mutable fun_returns = false
    val mutable proc_returns = false

    method! vstmt s = 
      match s with 
        | Stmt_FunReturn _ -> (fun_returns <- true); (returns <- (StmtSet.add s returns)); DoChildren 
        | Stmt_ProcReturn _ -> (proc_returns <- true); (returns <- (StmtSet.add s returns)); DoChildren 
        | _ -> DoChildren 

    method any_rets () = fun_returns || proc_returns
  end

(* Replaces a statement list with a a call to a function containing the same statements.

  If the statement list contains return statements then a return statement is returned. 
  (we are assuming no early returns). 

  We are also assuming that the statement list is an entire scope, i.e. no definitions 
  escape from the block.

  Typically the statement list sl here is the code block in an if/elseif/else branch. 

  returns (call stmt, ident * fun_sig)
 *)
  let stmt_list_to_function (c: split_ctx) (sl:stmt list)(*: stmt *) = 
    let in_return_context sl = let v = new find_returns  in
      visit_stmts v sl  |> ignore ; v#any_rets () in
    let swap (a,b) = (b,a) in
    let param_types = List.filter (fun (t, i) -> i <> state_var.ident) (List.map swap (Bindings.bindings c.bindings)) in
    let param_types = (state_var.typ,state_var.ident)::param_types in
    let returning = if (in_return_context sl) then c.return_type else Unit in
    let fname = new_name "split_fun" in
    let targs = [] in
    let args = List.map snd (List.tl param_types) in
    let funsig : sc_fun_sig = {rt=returning; arg_types=param_types; targs=targs; args=args; body=sl} in
    let new_funsig =  (fname , funsig) in
    let call_params = List.map (fun i -> Expr_TApply ((FIdent("as_ref", 0)), [], [Expr_Var i])) args in
    let new_stmt = (match returning with
      | Immutable x -> Some None
      | Mutable x -> Some None
      | Infer  -> Some None
      | Unit -> None)
     |> (function 
        | Some _ -> Stmt_FunReturn ((Expr_TApply (fname, targs, call_params)), Unknown)
        | None -> Stmt_TCall (fname,targs,call_params, Unknown))
    in (new_stmt, new_funsig)

  (* Create a function containing a single return statement of the given expression. All variables used in the expression
    become parameters to the function. *)
  let expr_to_function (c: split_ctx) (e: expr)  = 
    let fname = new_name "split_expr" in
    let returning = Infer in
    let params =  List.map (fun e -> Option.map (fun v -> v, e) (Bindings.find_opt e c.bindings)) (IdentSet.elements (fv_expr e)) in
    let params = List.concat_map Option.to_list params in
    let params = (state_var.typ,state_var.ident)::List.filter (fun (t, i) -> i <> state_var.ident) params in
    let targs = [] in
    let args = List.map snd (List.tl params) in (* chop off state var since its always added to calls*)
    (* as_ref here is a bit of a hack *)
    let call_params = List.map (fun i -> Expr_TApply ((FIdent("as_ref", 0)), [], [Expr_Var i])) args in
    let body = [Stmt_FunReturn (e, Unknown)] in
    let funsig =  {rt=returning; arg_types=params; targs=targs; args=args; body=body} in
    let callexpr = Expr_TApply (fname, [], call_params) in
    (callexpr, (fname, funsig))


  (* When a scope block has many statements *)
  let outline_function_on_size_threshold ctx sl thresh = 
    let sum x = List.fold_left (fun a b -> (a + b)) 0 x in
    let branch_weight = compose count_stmts_list sum in
    let stmt_weights = sl |> List.map (fun s -> match s with 
      | Stmt_If(c, t, els, f, loc) -> (s, [branch_weight t] 
        @ (List.map (branch_weight) (List.map (function | S_Elsif_Cond (e, sl) -> sl) els)) 
        @ [branch_weight f]) 
      | c -> (s, [count_stmts c] )
    ) in
    let total = List.map (compose snd sum) stmt_weights |> sum  in
    if (total < thresh) then (sl, []) else 
      let (f,s) = stmt_list_to_function ctx sl 
      in ([f], [s])

  (* When a function has many statements at the same level. *)
  let chunk_outline_stmtlist_on_size_threshold ctx sl thresh =
    (* Find all the definitions in the block, move them to the beginning of the block, split the 
       block into a number of chunks and outline those chunks as a function each. 

       Not needed with agressive expression outlining. *)
    ()

  (* When an expression has many subexpressions *)
  let outline_expr_on_size_threshold ctx (thresh:int) (e:expr) = 
    let c = new stmt_counter in 
    let c = c#expr_count e in
    if (c > thresh) then let (e,f) = expr_to_function ctx e in (e, [f]) else (e, [])

  class branch_outliner (funsig: sc_fun_sig) (outline_thresh:int) = object(this)
    inherit Asl_visitor.nopAslVisitor

    val scoped_bindings : var_type Transforms.ScopedBindings.t  = let x = Transforms.ScopedBindings.init () in
      List.iter (fun (t,i) -> add_bind x i t) (funsig.arg_types);
      push_scope x ();
      x
    
    val mutable extra_funs : sc_fun_sig Bindings.t = Bindings.empty

    method add_fun (bs: (ident * sc_fun_sig) list) = extra_funs <- Bindings.union 
    (fun i a b -> if a == b then Some a else failwith ("(branch_outliner) split function names be distinct " ^ (name_of_ident i))) extra_funs (Bindings.of_seq (List.to_seq bs))  

    method split_sl sl = 
      let ctx = {return_type = funsig.rt; bindings = current_scope_bindings scoped_bindings} in
      let t,nf = outline_function_on_size_threshold ctx sl outline_thresh
      in this#add_fun nf;
      t

    method split_exp e = 
      let ctx = {return_type = funsig.rt; bindings = current_scope_bindings scoped_bindings} in
      let ne,nf = outline_expr_on_size_threshold ctx outline_thresh e 
      in this#add_fun nf;
      ne 


    method! enter_scope ss = push_scope scoped_bindings ()
    method! leave_scope ss = pop_scope scoped_bindings ()

    method! vstmt s =
      match s with
      | Stmt_VarDeclsNoInit(ty, vs, loc) ->
        List.iter (fun f -> add_bind scoped_bindings f (Mutable ty)) vs; 
        DoChildren
      | Stmt_VarDecl(ty, v, i, loc) ->
        add_bind scoped_bindings v (Mutable ty) ;
        DoChildren
      | Stmt_ConstDecl(ty, v, i, loc) ->
        add_bind scoped_bindings v (Immutable ty) ;
        DoChildren
      | Stmt_If (c, t, els, e, loc) ->
      ChangeDoChildrenPost ([Stmt_If (c, t, els, e, loc)], (function 
      | [Stmt_If (c, t, els, e, loc)] ->
            let c'   = visit_expr this c in
            (* visit each statement list and then maybe outline it *)
            let t'   = visit_stmts this t in
            let t' = this#split_sl t' in
            let els' = mapNoCopy (visit_s_elsif this ) els in
            let e'   = visit_stmts this e in
            let e' = this#split_sl e' in
            [Stmt_If (c', t', els', e', loc)]
        | _ -> [s]
          )) 
      (* Statements with child scopes that shouldn't appear towards the end of transform pipeline *)
      | Stmt_Case _ -> failwith "(FixRedefinitions) case not expected"
      | Stmt_For _ -> failwith "(FixRedefinitions) for not expected"
      | Stmt_While _ -> failwith "(FixRedefinitions) while not expected"
      | Stmt_Repeat _ -> failwith "(FixRedefinitions) repeat not expected"
      | Stmt_Try _ -> failwith "(FixRedefinitions) try not expected"
      | _ -> DoChildren

    method! vs_elsif e = let c,e = match e with 
      | S_Elsif_Cond (c, sl) -> c,sl in
      let sl = visit_stmts this e in
      let sl = this#split_sl sl in
      ChangeTo (S_Elsif_Cond (c,sl))

    method! vexpr e = ChangeTo (this#split_exp e)
    (*the generated code is too large with this, type inference seems to do ok without *)
    (*method! vexpr e = match e with 
      | Expr_TApply _ -> DoChildren (* to preserve typing of fun returns *)
      | e -> ChangeTo (this#split_exp e) *)

    method split_function (x:unit) = let sl = visit_stmts this (funsig.body) in (sl , extra_funs)

    end

end


let rec write_stmt ?(primitive:bool=false) s st =
  match s with
  | Stmt_VarDeclsNoInit(ty, vs, loc) ->
      List.iter (define st (Mutable ty)) vs ;
      let e = default_value ty st in
      List.iter (fun v -> write_ref v (Mutable ty) e st) vs 

  | Stmt_VarDecl(ty, v, e, loc) ->
      define st (Mutable ty) v;
      let e = prints_expr e st in
      write_ref v (Mutable ty) e st

  | Stmt_ConstDecl(ty, v, e, loc) ->
      define st (Immutable ty) v;
      let e = prints_expr e st in
      write_let v (Immutable ty) e st

  | Stmt_Assign(l, r, loc) ->
      let e = prints_expr  r st in
      write_assign l e st

  | Stmt_TCall(f, tes, es, loc) ->
      let tes = List.map (fun e -> prints_expr e st) tes in
      let es = List.map (fun e -> prints_expr e st) es in
      write_call (name_of_ident f) tes es st

  | Stmt_FunReturn(e, loc) ->
      write_fun_return (prints_expr e st) st

  | Stmt_ProcReturn(loc) ->
      write_proc_return st

  | Stmt_Assert(e, loc) ->
      write_assert (prints_expr e st) st

  | Stmt_Throw _ ->
      write_unsupported st

  | Stmt_If(c, t, els, f, loc) ->
      let rec iter = function
      | S_Elsif_Cond(c,b)::xs ->
          write_if_elsif (prints_expr c st) st;
          write_stmts b st;
          iter xs
      | [] -> () in
      write_if_start (prints_expr c st) st;
      write_stmts t st;
      iter els;
      if f <> [] then (write_if_else st; write_stmts f st);
      write_if_end st

  | _ -> failwith @@ "write_stmt: " ^ (pp_stmt s);

and write_stmts ?(primitive:bool=false) s st  =
  inc_depth st;
  push_scope st;
  match s with
  | [] ->
      write_proc_return st;
      dec_depth st
  | x::xs ->
      write_stmt x st;
      List.iter (fun s ->
        write_seq st;
        write_stmt s st
      ) xs;
      dec_depth st;
    assert (not st.skip_seq)
  ; pop_scope st


let write_preamble imports opens st =
  Printf.fprintf st.oc "/* AUTO-GENERATED ASLp LIFTER FILE */\npackage lifter\n";
  List.iter (fun n ->
    Printf.fprintf st.oc "import %s\n" n) imports;
  List.iter (fun n ->
    Printf.fprintf st.oc "import %s._\n" n) opens;
  Printf.fprintf st.oc "\n"

open AST

let init_b (u:unit) = Transforms.ScopedBindings.init () 

let init_st : st = { indent = 0; skip_seq=false;  oc=stdout;  mutable_vars = init_b (); extra_functions = Bindings.empty}

let rinit_st oc st : st =  {st with indent = 0; skip_seq=false;  oc=oc;   mutable_vars = init_b ()}  

let build_args (tys: ((var_type * ident)  list)) targs args =
  let targs =  List.map (fun t -> name_of_ident t) targs in 
  (*let args =  List.map (fun t -> name_of_ident t) args in *)
  let ta = List.map (fun (t, i) -> (name_of_ident i) ^ ": " ^ (prints_arg_type t)) tys in
  "(" ^ (String.concat "," (targs@ta)) ^ ")" 

let print_write_fn  (name: AST.ident) ((ret_tyo: var_type), (argtypes: ((var_type * ident) list)), targs, args, _, body) st = 
  let open Transforms.ScopedBindings in
  (*update_funcalls {name=(name_of_ident name); 
    targs=(List.map (fun _ -> Some (Type_Constructor (Ident "integer"))) targs); 
    args = List.map (fun (t, _) -> Some t) (argtypes)} (st.output_fun) ; *)
  let wargs = build_args argtypes targs args in
  let ret = prints_arg_type ret_tyo in
  let ret = if (ret <> "") then (": " ^ ret) else "" in
  (* bind params *)
  push_scope st.mutable_vars () ; 
  List.iter (fun (a,b) -> add_bind st.mutable_vars b a) argtypes  ;

  Printf.fprintf st.oc "def %s[RTSym, RTLabel, BV <: RTSym] %s %s = {\n" (name_of_ident name) wargs ret;

  write_stmts body st;
  Printf.fprintf st.oc "\n}\n";

  (* unbind params *)
  pop_scope st.mutable_vars ()

let write_fn (name: AST.ident) (fn:sc_fun_sig) st = 
  if ((sl_complexity fn.body) < 1000) then (print_write_fn name (fn.rt, fn.arg_types, fn.targs, fn.args, (), fn.body) st) else
  let ol = new FunctionSplitter.branch_outliner fn 5 in
  let (nb,to_add) = ol#split_function () in
  st.extra_functions <- to_add ;
  print_write_fn name (fn.rt, fn.arg_types, fn.targs, fn.args, (), nb) st;
  Bindings.iter (fun n b -> print_write_fn n (b.rt, b.arg_types, b.targs, b.args, (), b.body) st) to_add
  
let lift_fsig (fs: Eval.fun_sig) : sc_fun_sig = 
  let assigned = assigned_vars_of_stmts (fnsig_get_body fs) in
  let params = List.map (fun (t,i) -> if (IdentSet.mem i assigned) then Mutable t,i else Immutable t, i) (fnsig_get_typed_args fs) in
  let rt = match (fnsig_get_rt fs) with
    | Some x -> Immutable x
    | None -> Unit
  in {rt=rt; arg_types=(state_var.typ,state_var.ident)::params; targs=(fnsig_get_targs fs); args=(fnsig_get_args fs); body=(fnsig_get_body fs)}


(* Write an instruction file, containing just the behaviour of one instructions *)
let write_instr_file fn fnsig dir st =
  let m = name_of_FIdent fn in
  let path = dir ^ "/" ^ m ^ ".scala" in
  let oc = open_out path in
  let st = rinit_st oc st in
  write_preamble global_imports global_opens st;
  write_fn fn fnsig st;
  close_out oc;
  name_of_FIdent fn


(* Write the decoder file - should depend on all of the above *)
let write_decoder_file fn fnsig deps dir st =
  let m = "aslpOffline" in
  let path = dir ^ "/" ^ m ^ ".scala" in
  let oc = open_out path in
  let st = rinit_st oc st in
  write_preamble global_imports (global_opens) st;
  write_fn fn fnsig st;
  close_out oc;
  m 

(* Write the test file, containing all decode tests *)
let write_test_file tests dir st =
  let m = "decode_tests" in
  let path = dir ^ "/" ^ m ^".scala" in
  let oc = open_out path in
  let st = rinit_st oc st in
  write_preamble global_imports (global_opens) st;
  Bindings.iter (fun i s -> write_fn i (lift_fsig s) st) tests;
  close_out oc;
  m

let run (dfn : ident) (dfnsig : ty option * 'a * ident list * ident list * 'b * stmt list) (tests : (ty option * 'a * ident list * ident list * 'b * stmt list) Bindings.t) (fns : (ty option * 'a * ident list * ident list * 'b * stmt list) Bindings.t) (dir : typeid) =
  let st = init_st  in
  let files = Bindings.fold (fun fn fnsig acc -> (write_instr_file fn (lift_fsig fnsig) dir st)::acc) fns [] in
  let files = (write_test_file tests dir st)::files in
  write_decoder_file dfn (lift_fsig dfnsig) files dir st |> ignore ; 
  ()

