type id = Symbol.t [@@deriving show, eq]
type ptr = int [@@deriving show, eq]

type typ =
  | Tvar of id
  | Tunit
  | Tbool
  | Tuint
  | Tprod of typ list
  | Tptr of typ option
[@@deriving show, eq]

type value =
  | Vvar of id
  | Vunit
  | Vnum of int
  | Vbool of bool
  | Vnull
  | Vptr of typ * ptr
  | Vdefault of typ
  | Vprod of value list
[@@deriving show, eq]

type uop =
  | Unot
  | Utest
  | Ulnot
[@@deriving show, eq]

type bop =
  | Band
  | Bor
  | Bplus
  | Bminus
  | Btimes
  | Beq
  | Bneq
  | Bless
  | Bgreater
  | Ble
  | Bge
  | Bland
  | Blor
  | Blsl
  | Blsr
[@@deriving show, eq]

type bound =
  | Ovar of id
  | Ominus of id * int
  | Oconst of int
[@@deriving show, eq]

type call =
  { name : id
  ; bound : bound option
  ; args : value list
  }
[@@deriving show, eq]

type exp =
  | Eval of value
  | Eprod of exp list
  | Eproj of exp * int
  | Euop of uop * exp
  | Ebop of bop * exp * exp
  | Ealloc of typ
  | Efun of call
[@@deriving show, eq]

type pat =
  | Pid of id
  | Punit
  | Puint of int
  | Pbool of bool
  | Pnull
  | Pprod of pat list
[@@deriving show, eq]

type stmt =
  | Sseq of stmt list
  | Sassign of pat * exp
  | Sunassign of pat * exp
  | Sswap of id * id
  | Smem_swap of exp * id
  | Sif of id * stmt * stmt
  | Sdebug
  | Swith of stmt * stmt
[@@deriving show, eq]

type id_typ =
  { name : id
  ; typ : typ
  }

type func =
  { name : id
  ; bound : bound option
  ; args : id_typ list
  ; result : value * typ
  ; body : stmt
  }

type static =
  { name : id
  ; elt_type : typ
  ; size : int
  ; values : value list option
  }

type decl =
  | Dstatic of static
  | Dtype of id_typ
  | Dfunc of func
