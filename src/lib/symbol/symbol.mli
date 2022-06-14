module Table = Core.Int.Table
module Map = Core.Int.Map
module Set = Core.Int.Set

type t = int [@@deriving show, eq]

val get_sym : string -> t
val name : t -> string
val nonce : unit -> t
