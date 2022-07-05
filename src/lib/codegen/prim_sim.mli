module State = Core.Int.Table
module Prim_circuit := Prim_circuit_

type state = bool State.t

val sim_gate : state -> Prim_circuit.gate -> unit
val sim_gates : state -> Prim_circuit.Gates.t -> unit

val interp : Prim_circuit.modul list -> state
val show_reg : state -> string
