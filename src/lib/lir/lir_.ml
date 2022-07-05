type id = Symbol.t [@@deriving show, eq]

type typ =
  | Tunit
  | Tuint
  | Tbool
  | Tprod of typ list
[@@deriving show, eq]

type value =
  | Vvar of id
  | Vunit
  | Vnum of int
  | Vbool of bool
  | Vprod of value list
[@@deriving show, eq]

type uop = Ulnot [@@deriving show, eq]

type bop =
  | Bland
  | Blor
  | Beq
  | Blsl
  | Blsr
  | Bplus
  | Bminus
  | Btimes
  | Bless
[@@deriving show, eq]

type exp =
  | Eval of value
  | Eproj of id * typ list * int
  | Euop of uop * id
  | Ebop of bop * id * id
  | Ealloc of typ
[@@deriving show, eq]

type stmt =
  | Sassign of typ * id * exp
  | Sunassign of typ * id * exp
  | Sswap of id * id
  | Smem_swap of id * id
  | Sif of id * stmt list
[@@deriving show, eq]

type modul =
  { name : id
  ; out_arg : typ * id
  ; args : (typ * id) list
  ; body : stmt list
  }
[@@deriving show, eq]
