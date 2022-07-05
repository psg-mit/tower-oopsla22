open Core
module Prim_circuit = Prim_circuit_
open Prim_circuit_
module WireSet = Int.Set

let num n ws =
  Gates.list
  @@ List.filter_mapi ws ~f:(fun i w ->
         if n land (1 lsl i) <> 0 then Some (Pnot (w, [])) else None)
;;

let with_uncomputed g1 g2 = Gates.concat [ Gates.concat [ g1; g2 ]; Gates.rev g1 ]

let with_inverted ws g =
  let inv = Gates.list @@ List.map ws ~f:(fun w -> Pnot (w, [])) in
  with_uncomputed inv g
;;

let cond = Gates.cond

let swap ws1 ws2 =
  Gates.concat
  @@ List.map2_exn ws1 ws2 ~f:(fun w1 w2 ->
         Gates.list [ Pnot (w1, [ w2 ]); Pnot (w2, [ w1 ]); Pnot (w1, [ w2 ]) ])
;;

let copy ws1 ws2 = Gates.list @@ List.map2_exn ~f:(fun w1 w -> Pnot (w, [ w1 ])) ws1 ws2

let land_ ws1 ws2 ws =
  Gates.list @@ List.map3_exn ~f:(fun w1 w2 w -> Pnot (w, [ w1; w2 ])) ws1 ws2 ws
;;

let lor_ ws1 ws2 ws =
  Gates.concat
  @@ List.map3_exn ws1 ws2 ws ~f:(fun w1 w2 w ->
         Gates.list [ Pnot (w, [ w1 ]); Pnot (w, [ w2 ]); Pnot (w, [ w1; w2 ]) ])
;;

let add a ws1 ws2 ws =
  let n = List.length ws in
  let anc = Alloc.alloc_n a n in
  let anc2 = Alloc.alloc_n a n in
  let wss = List.zip_exn (List.zip_exn ws1 ws2) (List.zip_exn anc2 anc) in
  let g =
    Gates.concat
    @@ List.mapi wss ~f:(fun i ((w1, w2), (w, a)) ->
           match i with
           | 0 -> Gates.list [ Pnot (a, [ w1; w2 ]); Pnot (w, [ w1 ]); Pnot (w, [ w2 ]) ]
           | i when i = n - 1 ->
             let a' = List.nth_exn anc (i - 1) in
             Gates.list [ Pnot (w, [ w1 ]); Pnot (w, [ w2 ]); Pnot (w, [ a' ]) ]
           | i ->
             let a' = List.nth_exn anc (i - 1) in
             Gates.list
               [ Pnot (a, [ w1; w2 ])
               ; Pnot (a, [ w1; a' ])
               ; Pnot (a, [ w2; a' ])
               ; Pnot (w, [ w1 ])
               ; Pnot (w, [ w2 ])
               ; Pnot (w, [ a' ])
               ])
  in
  with_uncomputed g @@ copy anc2 ws
;;

let sub a ws1 ws2 ws =
  let n = List.length ws in
  let anc = Alloc.alloc_n a n in
  let anc2 = Alloc.alloc_n a n in
  let wss = List.zip_exn (List.zip_exn ws1 ws2) (List.zip_exn anc2 anc) in
  let g =
    Gates.concat
    @@ List.mapi wss ~f:(fun i ((w1, w2), (w, b)) ->
           match i with
           | 0 ->
             Gates.concat
               [ with_inverted [ w1 ] @@ Gates.list [ Pnot (b, [ w1; w2 ]) ]
               ; Gates.list [ Pnot (w, [ w1 ]); Pnot (w, [ w2 ]) ]
               ]
           | i when i = n - 1 ->
             let b' = List.nth_exn anc (i - 1) in
             Gates.list [ Pnot (w, [ w1 ]); Pnot (w, [ w2 ]); Pnot (w, [ b' ]) ]
           | i ->
             let b' = List.nth_exn anc (i - 1) in
             let l =
               with_inverted [ w1 ]
               @@ Gates.list
                    [ Pnot (b, [ w1; w2 ]); Pnot (b, [ w1; b' ]); Pnot (b, [ w2; b' ]) ]
             in
             Gates.concat
               [ l; Gates.list [ Pnot (w, [ w1 ]); Pnot (w, [ w2 ]); Pnot (w, [ b' ]) ] ])
  in
  with_uncomputed g @@ copy anc2 ws
;;

let mul a ws1 ws2 ws =
  let n = List.length ws in
  let zeroes = Alloc.alloc_n a n in
  let g, res =
    List.foldi
      ws2
      ~init:(Gates.list [], Alloc.alloc_n a n)
      ~f:(fun i (g, acc) w2 ->
        let shifted = List.sub zeroes ~pos:0 ~len:i @ List.sub ws1 ~pos:0 ~len:(n - i) in
        let anc = Alloc.alloc_n a n in
        let c1 = cond w2 (add a shifted acc anc) in
        let c2 = with_inverted [ w2 ] (cond w2 (copy acc anc)) in
        Gates.concat [ g; c1; c2 ], anc)
  in
  with_uncomputed g @@ copy res ws
;;

let le a ws1 ws2 w =
  let init =
    let w = Alloc.alloc a in
    Gates.list [ Pnot (w, []) ], w
  in
  let g, w' =
    List.fold2_exn ws1 ws2 ~init ~f:(fun (g, w) w1 w2 ->
        let a1 = Alloc.alloc a in
        let e =
          Gates.concat
            [ Gates.list [ Pnot (a1, [ w1; w2; w ]) ]
            ; with_inverted [ w1; w2 ] @@ Gates.list [ Pnot (a1, [ w1; w2; w ]) ]
            ]
        in
        let a2 = Alloc.alloc a in
        let l = with_inverted [ w1 ] @@ Gates.list [ Pnot (a2, [ w1; w2 ]) ] in
        let w = Alloc.alloc a in
        let o = lor_ [ a1 ] [ a2 ] [ w ] in
        Gates.concat [ g; e; l; o ], w)
  in
  with_uncomputed g @@ copy [ w' ] [ w ]
;;

let rot ws i =
  let n = List.length ws in
  let i = (n + (i mod n)) mod n in
  assert (n mod i = 0);
  Gates.concat
  @@ List.concat
  @@ List.init i ~f:(fun j ->
         List.init
           ((n / i) - 1)
           ~f:(fun k ->
             swap
               [ List.nth_exn ws @@ ((k * i) + j) ]
               [ List.nth_exn ws @@ (((k + 1) * i) + j) ]))
;;

let rotr ws1 ws2 =
  Gates.concat
  @@ List.mapi ws2 ~f:(fun i w1 ->
         let i = 1 lsl i in
         if i < List.length ws1 then cond w1 (rot ws1 i) else Gates.list [])
;;

let rotl ws1 ws2 = Gates.rev (rotr ws1 ws2)

let one_hot ws1 ws2 =
  let n = List.length ws1 in
  let k = Int.ceil_log2 n in
  Gates.concat
  @@ List.mapi (List.sub ~pos:0 ~len:k ws2) ~f:(fun i w ->
         if i = 0
         then
           Gates.list
             [ Pnot (List.nth_exn ws1 0, [])
             ; Pnot (List.nth_exn ws1 1, [ w ])
             ; Pnot (List.nth_exn ws1 0, [ List.nth_exn ws1 1 ])
             ]
         else
           Gates.concat
           @@ List.init (1 lsl i) ~f:(fun n ->
                  let w' = List.nth_exn ws1 (n + (1 lsl i)) in
                  Gates.list
                    [ Pnot (w', [ w; List.nth_exn ws1 n ])
                    ; Pnot (List.nth_exn ws1 n, [ w' ])
                    ]))
;;

let prop_left ws1 =
  Gates.concat
  @@ List.mapi ws1 ~f:(fun i w ->
         if i = 0
         then Gates.list []
         else Gates.list [ Pnot (w, [ List.nth_exn ws1 (i - 1) ]) ])
;;

let maskl ws1 ws2 = Gates.concat [ one_hot ws1 ws2; prop_left ws1 ]

let binary_op a bop ws1 ws2 ws =
  (* TODO: statically allocate ancillas for ops *)
  (* TODO: test mem_swap and init_mem *)
  let n = List.length ws in
  match bop with
  | Lir.Bland -> land_ ws1 ws2 ws
  | Blor -> lor_ ws1 ws2 ws
  | Beq ->
    let w =
      match ws with
      | [ w ] -> w
      | _ -> assert false
    in
    let a1 = Alloc.alloc a in
    let a2 = Alloc.alloc a in
    let le1 = le a ws1 ws2 a1 in
    let le2 = le a ws2 ws1 a2 in
    with_uncomputed (Gates.concat [ le1; le2 ]) @@ land_ [ a1 ] [ a2 ] [ w ]
  | Bless ->
    let w =
      match ws with
      | [ w ] -> w
      | _ -> assert false
    in
    Gates.concat [ le a ws2 ws1 w; Gates.list [ Pnot (w, []) ] ]
  | Blsl ->
    let anc1 = Alloc.alloc_n a n in
    let c = copy ws1 anc1 in
    let r = rotl anc1 ws2 in
    let anc2 = Alloc.alloc_n a n in
    let m = maskl anc2 ws2 in
    let m2 = land_ anc1 anc2 ws in
    with_uncomputed (Gates.concat [ c; r; m ]) @@ m2
  | Blsr ->
    let anc1 = Alloc.alloc_n a n in
    let c = copy ws1 anc1 in
    let r = rotr anc1 ws2 in
    let anc2 = Alloc.alloc_n a n in
    let m = maskl anc2 ws2 in
    let m2 = land_ anc1 (List.rev anc2) ws in
    with_uncomputed (Gates.concat [ c; r; m ]) @@ m2
  | Bplus -> add a ws1 ws2 ws
  | Bminus -> sub a ws1 ws2 ws
  | Btimes -> mul a ws1 ws2 ws
;;

let mem_swap ws1 ws2 ~mem:(anc, mem) ~cell_size =
  let o = one_hot anc ws1 in
  let s =
    Gates.concat
    @@ List.map2_exn (List.chunks_of mem ~length:cell_size) anc ~f:(fun cell i ->
           cond i @@ swap (List.sub ~pos:0 ~len:(List.length ws2) cell) ws2)
  in
  with_uncomputed o s
;;

let init_mem a ~hp cell_size num_cells =
  let ctrl = List.init num_cells ~f:(fun _ -> Alloc.alloc a) |> List.rev in
  let cells =
    List.init num_cells ~f:(fun _ ->
        List.init cell_size ~f:(fun _ -> Alloc.alloc a) |> List.rev)
    |> List.rev
  in
  let cs =
    Gates.concat
    @@ List.mapi cells ~f:(fun i cell ->
           num (if i = 0 || i = num_cells - 1 then 0 else i + 1) cell)
  in
  Gates.concat [ num 1 hp; cs ], (ctrl, List.concat cells)
;;

let rec wires = function
  | Circuit.Ginit w -> WireSet.singleton w
  | Gcond (_, g) | Gconj g -> List.map g ~f:wires |> WireSet.union_list
  | Gnum _ | Gbool _ | Gnot _ | Gcopy _ | Gbop _ | Gswap _ | Gmem_swap _ -> WireSet.empty
;;

let rec lower_gate a ~mem ~cell_size = function
  | Circuit.Gbool (b, w) -> if b then Gates.list [ Pnot (w, []) ] else Gates.list []
  | Gnum (n, ws) -> num n ws
  | Gnot ws -> Gates.list @@ List.map ws ~f:(fun w -> Pnot (w, []))
  | Gcopy (ws1, ws2) -> copy ws1 ws2
  | Gbop (bop, ws1, ws2, ws) -> binary_op a bop ws1 ws2 ws
  | Gswap (ws1, ws2) -> swap ws1 ws2
  | Gmem_swap (ws1, ws2) -> mem_swap ws1 ws2 ~mem ~cell_size
  | Gcond (c, g) -> Gates.concat @@ List.map ~f:(lower_gate a ~mem ~cell_size) g |> cond c
  | Gconj g -> Gates.rev @@ Gates.concat @@ List.map ~f:(lower_gate a ~mem ~cell_size) g
  | Ginit _ -> Gates.list []
;;

let lower_module (m : Circuit.modul) ~num_cells =
  let a = Alloc.empty () in
  List.map ~f:wires m.body |> WireSet.union_list |> Set.iter ~f:(Alloc.mark_allocated a);
  List.iter m.hp ~f:(Alloc.mark_allocated a);
  let c, mem = init_mem a ~hp:m.hp m.cell_size num_cells in
  let body =
    Gates.concat
      [ c; Gates.concat @@ List.map ~f:(lower_gate a ~mem ~cell_size:m.cell_size) m.body ]
  in
  { name = m.name; hp = m.hp; mem; out_args = m.out_args; args = m.out_args; body }
;;
