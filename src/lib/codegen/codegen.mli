module Prim_circuit := Prim_circuit_

(* Little-endian encoding of an integer into wires *)
val num : int -> Prim_circuit.wire list -> Prim_circuit.Gates.t
val with_uncomputed : Prim_circuit.Gates.t -> Prim_circuit.Gates.t -> Prim_circuit.Gates.t
val with_inverted : Prim_circuit.wire list -> Prim_circuit.Gates.t -> Prim_circuit.Gates.t
val cond : Prim_circuit.wire -> Prim_circuit.Gates.t -> Prim_circuit.Gates.t
val swap : Prim_circuit.wire list -> Prim_circuit.wire list -> Prim_circuit.Gates.t
val copy : Prim_circuit.wire list -> Prim_circuit.wire list -> Prim_circuit.Gates.t

val land_
  :  Prim_circuit.wire list
  -> Prim_circuit.wire list
  -> Prim_circuit.wire list
  -> Prim_circuit.Gates.t

val lor_
  :  Prim_circuit.wire list
  -> Prim_circuit.wire list
  -> Prim_circuit.wire list
  -> Prim_circuit.Gates.t

val add
  :  Alloc.t
  -> Prim_circuit.wire list
  -> Prim_circuit.wire list
  -> Prim_circuit.wire list
  -> Prim_circuit.Gates.t

val sub
  :  Alloc.t
  -> Prim_circuit.wire list
  -> Prim_circuit.wire list
  -> Prim_circuit.wire list
  -> Prim_circuit.Gates.t

val rotl : Prim_circuit.wire list -> Prim_circuit.wire list -> Prim_circuit.Gates.t
val rotr : Prim_circuit.wire list -> Prim_circuit.wire list -> Prim_circuit.Gates.t
val maskl : Prim_circuit.wire list -> Prim_circuit.wire list -> Prim_circuit.Gates.t

val binary_op
  :  Alloc.t
  -> Circuit.bop
  -> Prim_circuit.wire list
  -> Prim_circuit.wire list
  -> Prim_circuit.wire list
  -> Prim_circuit.Gates.t

val mem_swap
  :  Prim_circuit.wire list
  -> Prim_circuit.wire list
  -> mem:Prim_circuit.wire list * Prim_circuit.wire list
  -> cell_size:int
  -> Prim_circuit.Gates.t

val init_mem
  :  Alloc.t
  -> hp:Prim_circuit.wire list
  -> int
  -> int
  -> Prim_circuit.Gates.t * (Prim_circuit.wire list * Prim_circuit.wire list)

val lower_gate
  :  Alloc.t
  -> mem:Prim_circuit.wire list * Prim_circuit.wire list
  -> cell_size:int
  -> Circuit.gate
  -> Prim_circuit.Gates.t

val lower_module : Circuit.modul -> num_cells:int -> Prim_circuit.modul
