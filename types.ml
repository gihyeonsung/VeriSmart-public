::::::::::::::
verify/report.ml
::::::::::::::
module QMap = struct                                                                        
  module BatMap = BatMap.Make (struct type t = Query.src let compare = Query.compare_src end)

  type t = v BatMap.t
  and k = Query.src
  and v = status * int
end
::::::::::::::
frontend/lang.ml
::::::::::::::
module Node = struct
  type t = ENTRY | EXIT | Node of int
end

type node = Node.t

module G = Graph.Persistent.Digraph.Concrete (Node)
module Scc = Graph.Components.Make(G)

type pgm = contract list
and contract = id * state_var_decl list * structure list * enum list * func list * cinfo

and state_var_decl = id * exp option * vinfo

and structure = id * var_decl list
and var_decl = id * vinfo

and enum = id * enum_mem list
and enum_mem = id

and func = id * param list * ret_param list * stmt * finfo
and param = id * vinfo
and ret_param = id * vinfo

and fsig = id * typ list
and fkey = id * id * typ list
and func_decl = id * var list * var list 
and visibility = Public | Internal | External | Private

and var = id * typ

and stmt =
  | Assign of lv * exp * loc
  | Decl of lv
  | Seq of stmt * stmt
  | Call of lv option * exp * exp list * loc * int (* int for call-site id *) 
  | Skip
  | If of exp * stmt * stmt
  | While of exp * stmt
  | Break
  | Continue
  | Return of exp option * loc
  | Throw
  | Assume of exp * loc
  | Assert of exp * loc (* used to check safety conditions *)
  | Assembly of (id * int) list * loc 

and exp =
  | Int of BatBig_int.t
  | Real of float 
  | Str of string
  | Lv of lv
  | Cast of typ * exp
  | BinOp of bop * exp * exp * einfo
  | UnOp of uop * exp * typ
  | True | False
  | ETypeName of elem_typ (* may be second arguments of abi.decode functions *)
  (* exists only in the interim process *)
  | IncTemp of exp * bool * loc (* true if prefix, false if postfix *)
  | DecTemp of exp * bool * loc
  | CallTemp of exp * exp list * einfo
  | CondTemp of exp * exp * exp * typ * loc
  | AssignTemp of lv * exp * loc

and bop =
  | Add | Sub | Mul | Div | Mod | Exponent
  | GEq | Gt | LEq | Lt | LAnd | LOr | Eq | NEq
  | ShiftL | ShiftR | BXor | BAnd | BOr

and uop =
  | Pos | Neg | LNot | BNot

and lv =
  | Var of (id * vinfo)
  | MemberAccess of exp * id * vinfo * typ (* exp.id / vinfo is for id *)
  | IndexAccess of exp * exp option * typ (* exp[exp?] *)
  | Tuple of exp option list * typ (* [a, b, c, d, ] *)

and id = string
and loc = int 

and cinfo = {
  numid : int;
  inherit_order : int list;
  lib_typ_lst : (id * typ) list; (* a list of pairs of (lib name, aliased type). Orders do not matter. *)
  ckind : string
}

and vinfo = {
  vloc : loc;
  is_gvar : bool;
  vtype : typ;
  vvis : visibility;
  vid : int;
  refid : int; (* referenced declartion. valid only for non-function variables *)
  storage : string; 
  flag : bool; (* true if the information is propagated *)
  org : string (* original name (source code) before renamed or replaced *)
}

and einfo = {
  eloc : loc; 
  etyp : typ;
  eid : int
}

and finfo = {
  is_constructor : bool;
  is_payable : bool;
  fvis : visibility;
  fid : int;
  scope : int; (* belonging contract numid *)
  scope_s : id; (* belonging contract name *)
  cfg : cfg
}

and cfg = {
  graph         : G.t;
  pre_set       : node BatSet.t; (* nodes just before loop headers *)
  lh_set        : node BatSet.t; (* loop header set *)
  lx_set        : node BatSet.t; (* loop exit set *)
  continue_set  : node BatSet.t;
  break_set     : node BatSet.t;
  basic_paths   : node list BatSet.t;
  stmt_map      : (node, stmt) BatMap.t;
  signature     : fkey
}

and typ =
  | ConstInt
  | ConstString
  | ConstReal
  | EType of elem_typ
  | Mapping of elem_typ * typ
  | Mapping2 of typ * typ
  | Array of typ * int option (* type, (size)? *)
  | Contract of id
  | Struct of id
  | Enum of id
  | TupleType of typ list
  | Void (* dummy type *)

and elem_typ =
  | Address
  | Bool
  | String
  | UInt of int
  | SInt of int
  | Bytes of int (* fixed-size byte arrays *)
  | DBytes (* dynamically-sized byte arrays *) 
  (* | Fixed | UFixed *)

module OrderedType = struct
  type t = BatBig_int.t
  let compare = BatBig_int.compare
end

module BigIntSet = BatSet.Make (OrderedType)
::::::::::::::
verify/model.ml
::::::::::::::
type t = (var, Z3.Expr.expr) BatMap.t
and model = t

module ModelMap = struct
  type t = (Node.t, model) BatMap.t 
end

******** t.ml: Not a text file ********

::::::::::::::
verify/verifier.ml
::::::::::::::
module Workset = struct
  type work = InvMap.t 
  
  module OrderedType = struct
    type t = work
  end
  
  module Heap = BatHeap.Make (OrderedType)
  
  (* type of workset : heap * (string set) *)
  type t = Heap.t * string BatSet.t
end
::::::::::::::
verify/invMap.ml
::::::::::::::
type t = (Node.t, vformula) BatMap.t
and invmap
::::::::::::::
verify/component.ml
::::::::::::::
type t = comps
and comps = {
  mapvars : var BatSet.t;
  composites : ExpSet.t;
  ivars : var BatSet.t;
  avars : var BatSet.t;
  ints : BigIntSet.t
}
::::::::::::::
pre/structMap.ml
::::::::::::::
type t = (id, structure list) BatMap.t
and smap = t
::::::::::::::
pre/interval/itvDom.ml
::::::::::::::
module Loc = struct
  type t = id * typ
end

module Val = struct
  type t = Itv.t * GTaint.t * BTaint.t
end 

module Mem = struct 
  type t = (Loc.t, Val.t) BatMap.t
end
::::::::::::::
pre/global.ml
::::::::::::::
type t = {
  cnames   : string list;
  gvars    : var list;
  smap     : StructMap.t;
  enums    : enum list;
  fmap     : FuncMap.t;
  f_defuse : FuncDefUse.t;
  lhs      : Node.t BatSet.t
}
and global = t
::::::::::::::
pre/path.ml
::::::::::::::
type t = fkey * Node.t list
and path = t

module PathSet = BatSet.Make (struct type t = path let compare = Stdlib.compare end)
::::::::::::::
pre/funcDefUse.ml
::::::::::::::
type t = (fkey, value) BatMap.t
and value = def_set * use_set * use_set_assume 
and def_set = id BatSet.t
and use_set = id BatSet.t
and use_set_assume = id BatSet.t
::::::::::::::
checker/query.ml
::::::::::::::
type query = {
  vc: vformula;
  vc2: vformula;
  kind: kind;
  loc: loc;
  org_q: origin;
  path: Path.t;
  src_f: fkey;
  sc_src: string;
}

and status = Proven | UnProven | Disproven
and kind = IO | DZ | ACCESS | ASSERT | ERC20 | ERC721 | LEAK | SUICIDE
and origin = Org_Stmt of stmt | Org_Exp of exp | Org_Func of string

type src = kind * loc * string (* location in the original source code *)
::::::::::::::
pre/funcMap.ml
::::::::::::::
type t = ((cname*fname*typ list), func) BatMap.t
and cname = id
and fname = id
::::::::::::::
pre/interval/bTaint.ml
::::::::::::::
type t =
  | Top (* may be tainted from block information *)
  | Bot (* not taited by block information *)
::::::::::::::
verify/vlang.ml
::::::::::::::
type vformula =
  | VTrue | VFalse
  | VNot of vformula
  | VAnd of vformula * vformula
  | VOr of vformula * vformula
  | VBinRel of vbrel * vexp * vexp
  | Imply of vformula * vformula
  | SigmaEqual of vexp * var (* left: uint, right: mapping (address => uint) *)
  | NoOverFlow of var
  | ForAll of var list * vformula
  | Label of int * vformula (* 0: assignment, 1: assume *)

and vexp =
  | VInt of BatBig_int.t
  | VVar of var
  | Read of vexp * vexp * typ (* A[i] *)
  | Write of vexp * vexp * vexp (* A[i] := v, return A *)
  | VBinOp of vbop * vexp * vexp * typ
  | VUnOp of vuop * vexp * typ
  | VCast of typ * vexp
  | VCond of vformula
  | Ite of vexp * vexp * vexp

and vid = string 
and vbrel = VGeq | VGt | VEq
and vbop = VAdd | VSub | VMul | VDiv | VMod | VPower
           | VShiftL | VShiftR | VBXor | VBAnd | VBOr
and vuop = VNeg | VBNot 

module FormulaSet = BatSet.Make (struct type t = vformula let compare = compare end)
module ExpSet = BatSet.Make (struct type t = vexp let compare = compare_ve end)

module PartitionSet = BatSet.Make (struct type t = ExpSet.t let compare = ExpSet.compare let to_string = string_of_set (string_of_set to_string_vexp) end)
::::::::::::::
pre/interval/itv.ml
::::::::::::::
type t' = 
  | V of BatBig_int.t
  | PInf 
  | NInf

type t = Itv of t' * t' | Bot  
::::::::::::::
pre/interval/gTaint.ml
::::::::::::::
type t = var BatSet.t
