open Core

type t = int ref * int Queue.t

let empty () = ref 0, Queue.create ()

let alloc (r, q) =
  match Queue.dequeue q with
  | Some x -> x
  | None ->
    let x = !r in
    r := x + 1;
    x
;;

let alloc_n a n = List.init n ~f:(fun _ -> alloc a) |> List.rev
let dealloc (_, q) n = Queue.enqueue q n

let mark_allocated (r, q) x =
  Queue.clear q;
  if !r <= x then r := x + 1
;;
