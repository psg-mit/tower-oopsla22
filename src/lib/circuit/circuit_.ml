type wire = int [@@deriving show, eq]
type bop = Lir.bop [@@deriving show, eq]

type gate =
  | Gnum of int * wire list
  | Gbool of bool * wire
  | Gnot of wire list
  | Gcopy of wire list * wire list
  | Gbop of bop * wire list * wire list * wire list
  | Gswap of wire list * wire list
  | Gmem_swap of wire list * wire list
  | Gcond of wire * gate list
  | Gconj of gate list
  | Ginit of wire
[@@deriving show, eq, variants]

type modul =
  { name : Symbol.t
  ; hp : wire list
  ; out_args : wire list
  ; args : wire list
  ; body : gate list
  ; cell_size : int
  }
[@@deriving show, eq]
