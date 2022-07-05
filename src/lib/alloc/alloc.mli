type t

val empty : unit -> t
val alloc : t -> int
val alloc_n : t -> int -> int list
val dealloc : t -> int -> unit
val mark_allocated : t -> int -> unit
