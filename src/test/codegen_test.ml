open Core
open Lib
open Codegen
open Prim_circuit
open Prim_sim

let dump_ctx ctx =
  let max = State.keys ctx |> List.max_elt ~compare:Int.compare |> Option.value_exn in
  List.init (max + 1) ~f:(fun i -> i)
  |> List.map ~f:(fun i ->
         Int.to_string i
         ^ ": "
         ^ Bool.to_string (State.find_or_add ctx ~default:(fun _ -> false) i))
  |> String.concat ~sep:"\n"
  |> eprintf "%s\n%!"
;;

let ctx_equal s1 s2 =
  State.for_alli s2 ~f:(fun ~key ~data ->
      match State.find s1 key with
      | Some d -> Bool.equal d data
      | None -> not data)
  && State.for_alli s1 ~f:(fun ~key ~data ->
         match State.find s2 key with
         | Some d -> Bool.equal d data
         | None -> not data)
;;

let is_subset s1 s2 =
  State.for_alli s1 ~f:(fun ~key ~data ->
      match State.find s2 key with
      | Some d -> Bool.equal d data
      | None -> not data)
;;

let test_num_nbit n x =
  let a = Alloc.empty () in
  let w1 = Alloc.alloc_n a n in
  let y = num x w1 |> Gates.to_list in
  let expected =
    List.concat
    @@ List.init n ~f:(fun i -> if x land (1 lsl i) <> 0 then [ Pnot (i, []) ] else [])
  in
  assert (List.equal Prim_circuit.equal_gate y expected)
;;

let () =
  print_endline "Testing num";
  let n = 16 in
  (List.init (1 lsl n) ~f:(test_num_nbit n) : 'a list) |> ignore
;;

let () =
  print_endline "Testing land";
  let a = Alloc.empty () in
  let w1 = Alloc.alloc_n a 4 in
  let w2 = Alloc.alloc_n a 4 in
  let w3 = Alloc.alloc_n a 4 in
  let ctx1 =
    [ 0, true
    ; 1, true
    ; 2, false
    ; 3, false
    ; 4, true
    ; 5, false
    ; 6, true
    ; 7, false
    ; 8, false
    ; 9, false
    ; 10, false
    ; 11, false
    ]
    |> State.of_alist_exn
  in
  Prim_sim.sim_gates ctx1 (Codegen.binary_op a Lir.Bland w1 w2 w3);
  let ctx2 =
    [ 0, true
    ; 1, true
    ; 2, false
    ; 3, false
    ; 4, true
    ; 5, false
    ; 6, true
    ; 7, false
    ; 8, true
    ; 9, false
    ; 10, false
    ; 11, false
    ]
    |> State.of_alist_exn
  in
  assert (State.equal Bool.equal ctx1 ctx2)
;;

let () =
  print_endline "Testing lor";
  let a = Alloc.empty () in
  let w1 = Alloc.alloc_n a 4 in
  let w2 = Alloc.alloc_n a 4 in
  let w3 = Alloc.alloc_n a 4 in
  let ctx1 =
    [ 0, true
    ; 1, true
    ; 2, false
    ; 3, false
    ; 4, true
    ; 5, false
    ; 6, true
    ; 7, false
    ; 8, false
    ; 9, false
    ; 10, false
    ; 11, false
    ]
    |> State.of_alist_exn
  in
  Prim_sim.sim_gates ctx1 (Codegen.binary_op a Lir.Blor w1 w2 w3);
  let ctx2 =
    [ 0, true
    ; 1, true
    ; 2, false
    ; 3, false
    ; 4, true
    ; 5, false
    ; 6, true
    ; 7, false
    ; 8, true
    ; 9, true
    ; 10, true
    ; 11, false
    ]
    |> State.of_alist_exn
  in
  assert (State.equal Bool.equal ctx1 ctx2)
;;

let test_add_nbit n x y =
  let a = Alloc.empty () in
  let w1 = Alloc.alloc_n a n in
  let w2 = Alloc.alloc_n a n in
  let w3 = Alloc.alloc_n a n in
  let n0 = num x w1 in
  let n1 = num y w2 in
  let g = Codegen.binary_op a Lir.Bplus w1 w2 w3 in
  let ctx1 = State.create () in
  Prim_sim.sim_gates ctx1 (Gates.concat [ n0; n1; g ]);
  let ctx2 =
    List.init n ~f:(fun i -> i, x land (1 lsl i) <> 0)
    @ List.init n ~f:(fun i -> i + n, y land (1 lsl i) <> 0)
    @ List.init n ~f:(fun i -> i + (2 * n), (x + y) mod (1 lsl n) land (1 lsl i) <> 0)
    |> State.of_alist_exn
  in
  assert (ctx_equal ctx1 ctx2)
;;

let () =
  print_endline "Testing add";
  let n = 8 in
  (List.init (1 lsl n) ~f:(fun i -> List.init (1 lsl n) ~f:(test_add_nbit n i))
    : 'a list list)
  |> ignore
;;

let test_sub_nbit n x y =
  let a = Alloc.empty () in
  let w1 = Alloc.alloc_n a n in
  let w2 = Alloc.alloc_n a n in
  let w3 = Alloc.alloc_n a n in
  let n0 = num x w1 in
  let n1 = num y w2 in
  let g = Codegen.binary_op a Lir.Bminus w1 w2 w3 in
  let ctx1 = State.create () in
  Prim_sim.sim_gates ctx1 (Gates.concat [ n0; n1; g ]);
  let ctx2 =
    List.init n ~f:(fun i -> i, x land (1 lsl i) <> 0)
    @ List.init n ~f:(fun i -> i + n, y land (1 lsl i) <> 0)
    @ List.init n ~f:(fun i ->
          i + (2 * n), (x - y + (1 lsl n)) mod (1 lsl n) land (1 lsl i) <> 0)
    |> State.of_alist_exn
  in
  assert (ctx_equal ctx1 ctx2)
;;

let () =
  print_endline "Testing sub";
  let n = 8 in
  (List.init (1 lsl n) ~f:(fun i -> List.init (1 lsl n) ~f:(test_sub_nbit n i))
    : 'a list list)
  |> ignore
;;

let test_lt_nbit n x y =
  let a = Alloc.empty () in
  let w1 = Alloc.alloc_n a n in
  let w2 = Alloc.alloc_n a n in
  let w3 = Alloc.alloc a in
  let n0 = num x w1 in
  let n1 = num y w2 in
  let g = Codegen.binary_op a Lir.Bless w1 w2 [ w3 ] in
  let ctx1 = State.create () in
  Prim_sim.sim_gates ctx1 (Gates.concat [ n0; n1; g ]);
  let ctx2 =
    List.init n ~f:(fun i -> i, x land (1 lsl i) <> 0)
    @ List.init n ~f:(fun i -> i + n, y land (1 lsl i) <> 0)
    @ [ 2 * n, x < y ]
    |> State.of_alist_exn
  in
  assert (ctx_equal ctx1 ctx2)
;;

let () =
  print_endline "Testing lt";
  let n = 8 in
  (List.init (1 lsl n) ~f:(fun i -> List.init (1 lsl n) ~f:(test_lt_nbit n i))
    : 'a list list)
  |> ignore
;;

let test_eq_nbit n x y =
  let a = Alloc.empty () in
  let w1 = Alloc.alloc_n a n in
  let w2 = Alloc.alloc_n a n in
  let w3 = Alloc.alloc a in
  let n0 = num x w1 in
  let n1 = num y w2 in
  let g = Codegen.binary_op a Lir.Beq w1 w2 [ w3 ] in
  let ctx1 = State.create () in
  Prim_sim.sim_gates ctx1 (Gates.concat [ n0; n1; g ]);
  let ctx2 =
    List.init n ~f:(fun i -> i, x land (1 lsl i) <> 0)
    @ List.init n ~f:(fun i -> i + n, y land (1 lsl i) <> 0)
    @ [ 2 * n, x = y ]
    |> State.of_alist_exn
  in
  assert (ctx_equal ctx1 ctx2)
;;

let () =
  print_endline "Testing eq";
  let n = 8 in
  (List.init (1 lsl n) ~f:(fun i -> List.init (1 lsl n) ~f:(test_eq_nbit n i))
    : 'a list list)
  |> ignore
;;

let test_mul_nbit n x y =
  let a = Alloc.empty () in
  let w1 = Alloc.alloc_n a n in
  let w2 = Alloc.alloc_n a n in
  let w3 = Alloc.alloc_n a n in
  let n0 = num x w1 in
  let n1 = num y w2 in
  let g = Codegen.binary_op a Lir.Btimes w1 w2 w3 in
  let ctx1 = State.create () in
  Prim_sim.sim_gates ctx1 (Gates.concat [ n0; n1; g ]);
  let ctx2 =
    List.init n ~f:(fun i -> i, x land (1 lsl i) <> 0)
    @ List.init n ~f:(fun i -> i + n, y land (1 lsl i) <> 0)
    @ List.init n ~f:(fun i -> i + (2 * n), x * y mod (1 lsl n) land (1 lsl i) <> 0)
    |> State.of_alist_exn
  in
  assert (ctx_equal ctx1 ctx2)
;;

let () =
  print_endline "Testing mul";
  let n = 8 in
  (List.init (1 lsl n) ~f:(fun i -> List.init (1 lsl n) ~f:(test_mul_nbit n i))
    : 'a list list)
  |> ignore
;;

let test_lsl_nbit n x y =
  let a = Alloc.empty () in
  let w1 = Alloc.alloc_n a n in
  let w2 = Alloc.alloc_n a n in
  let w3 = Alloc.alloc_n a n in
  let n0 = num x w1 in
  let n1 = num y w2 in
  let g = Codegen.binary_op a Lir.Blsl w1 w2 w3 in
  let ctx1 = State.create () in
  Prim_sim.sim_gates ctx1 (Gates.concat [ n0; n1; g ]);
  let ctx2 =
    List.init n ~f:(fun i -> i, x land (1 lsl i) <> 0)
    @ List.init n ~f:(fun i -> i + n, y land (1 lsl i) <> 0)
    @ List.init n ~f:(fun i -> i + (2 * n), (x lsl y) mod (1 lsl n) land (1 lsl i) <> 0)
    |> State.of_alist_exn
  in
  assert (ctx_equal ctx1 ctx2)
;;

let () =
  print_endline "Testing lsl";
  let n = 8 in
  let k = Int.ceil_log2 n in
  (List.init (1 lsl n) ~f:(fun i -> List.init (1 lsl k) ~f:(test_lsl_nbit n i))
    : 'a list list)
  |> ignore
;;

let test_lsr_nbit n x y =
  let a = Alloc.empty () in
  let w1 = Alloc.alloc_n a n in
  let w2 = Alloc.alloc_n a n in
  let w3 = Alloc.alloc_n a n in
  let n0 = num x w1 in
  let n1 = num y w2 in
  let g = Codegen.binary_op a Lir.Blsr w1 w2 w3 in
  let ctx1 = State.create () in
  Prim_sim.sim_gates ctx1 (Gates.concat [ n0; n1; g ]);
  let ctx2 =
    List.init n ~f:(fun i -> i, x land (1 lsl i) <> 0)
    @ List.init n ~f:(fun i -> i + n, y land (1 lsl i) <> 0)
    @ List.init n ~f:(fun i -> i + (2 * n), (x lsr y) mod (1 lsl n) land (1 lsl i) <> 0)
    |> State.of_alist_exn
  in
  assert (ctx_equal ctx1 ctx2)
;;

let () =
  print_endline "Testing lsr";
  let n = 8 in
  let k = Int.ceil_log2 n in
  (List.init (1 lsl n) ~f:(fun i -> List.init (1 lsl k) ~f:(test_lsr_nbit n i))
    : 'a list list)
  |> ignore
;;

let test_mem n num_cells x =
  let a = Alloc.empty () in
  let w1 = Alloc.alloc_n a n in
  let w2 = Alloc.alloc_n a n in
  let hp = Alloc.alloc_n a n in
  let cell_size = n in
  let c, mem = Codegen.init_mem a ~hp cell_size num_cells in
  let n0 = Codegen.num x w1 in
  let g = Codegen.lower_gate a ~mem ~cell_size:8 @@ Gmem_swap (w1, w2) in
  let ctx1 = State.create () in
  Prim_sim.sim_gates ctx1 (Gates.concat [ c; n0; g ]);
  let ctx2 =
    List.init n ~f:(fun i -> i, x land (1 lsl i) <> 0)
    @ List.init n ~f:(fun i -> i + n, (x + 1) land (1 lsl i) <> 0)
    |> State.of_alist_exn
  in
  assert (is_subset ctx2 ctx1);
  Prim_sim.sim_gates ctx1 g;
  let ctx2 =
    List.init n ~f:(fun i -> i, x land (1 lsl i) <> 0)
    @ List.init n ~f:(fun i -> i + n, false)
    |> State.of_alist_exn
  in
  assert (is_subset ctx2 ctx1)
;;

let () =
  print_endline "Testing mem";
  let n = 8 in
  let num_cells = 256 in
  (List.init (num_cells - 2) ~f:(fun i -> test_mem n num_cells (i + 1)) : 'a list)
  |> ignore
;;

let () = print_endline "Unit tests passed"
