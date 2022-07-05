open Core

let verbose = ref false
let interp = ref false
let interp_lir = ref false
let interp_circuit = ref false
let interp_prim = ref false
let inline = ref false
let compile = ref false
let print = ref false
let bounds = String.Table.create ()
let no_prim = ref false
let word_size = ref 8
let mem_size = ref 16

let process_bound s =
  match String.split ~on:':' s with
  | [ name; i ] -> String.Table.add_exn bounds ~key:name ~data:(Int.of_string i)
  | _ -> failwith "Expected function:bound"
;;

let speclist =
  [ "-v", Arg.Set verbose, "Run in verbose mode."
  ; "-i", Arg.Set interp, "Run the classical interpreter."
  ; "--interp_lir", Arg.Set interp_lir, "Run the LIR interpreter."
  ; "--interp_circuit", Arg.Set interp_circuit, "Run the circuit interpreter."
  ; "--interp_prim", Arg.Set interp_prim, "Run the primitive circuit interpreter."
  ; "--inline", Arg.Set inline, "Run the inlining pass before the interpreter."
  ; "-c", Arg.Set compile, "Compile the program to a circuit."
  ; "-p", Arg.Set print, "Print the compiler and interpreter outputs."
  ; "-b", Arg.String process_bound, "Set the bound for a given function."
  ; "--no-prim", Arg.Set no_prim, "Do not instantiate primitive circuits."
  ; "--word_size", Arg.Set_int word_size, "Set the word size."
  ]
;;
