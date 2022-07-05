open Core

type wire = int [@@deriving show, eq]
type gate = Pnot of wire * wire list [@@deriving show, eq, variants]

module Gates : sig
  type t [@@deriving show, eq]

  val to_list : t -> gate list
  val to_seq : t -> gate Sequence.t
  val length : t -> int
  val list : gate list -> t
  val concat : t list -> t
  val rev : t -> t
  val cond : wire -> t -> t
  val iter : f:(gate -> unit) -> t -> unit
end = struct
  type t =
    | List of gate list
    | Concat of t list
    | Rev of t
    | Cond of wire * t
  [@@deriving show, eq]

  let rec length = function
    | List l -> List.length l
    | Concat gs -> List.sum (module Int) gs ~f:length
    | Rev g -> length g
    | Cond (_, g) -> length g
  ;;

  let rec to_list = function
    | List l -> l
    | Concat gs -> List.concat @@ List.map ~f:to_list gs
    | Rev g -> List.rev @@ to_list g
    | Cond (c, g) ->
      List.map ~f:(function Pnot (w, cs) -> Pnot (w, c :: cs)) @@ to_list g
  ;;

  let rec to_seq = function
    | List l -> Sequence.of_list l
    | Concat gs -> Sequence.concat @@ Sequence.map ~f:to_seq @@ Sequence.of_list gs
    | Rev g -> Sequence.of_list @@ Sequence.to_list_rev @@ to_seq g
    | Cond (c, g) ->
      Sequence.map ~f:(function Pnot (w, cs) -> Pnot (w, c :: cs)) @@ to_seq g
  ;;

  let list l = List l
  let concat gs = Concat gs
  let rev g = Rev g
  let cond c g = Cond (c, g)

  let rec iter' ~f fwd cs = function
    | List l ->
      let l =
        List.map
          ~f:(function
            | Pnot (w, cs') -> Pnot (w, cs @ cs'))
          l
      in
      if fwd then List.iter ~f l else List.iter ~f @@ List.rev l
    | Concat gs ->
      let gs = if fwd then gs else List.rev gs in
      List.iter ~f:(iter' ~f fwd cs) gs
    | Rev g -> iter' ~f (not fwd) cs g
    | Cond (c, g) -> iter' ~f fwd (c :: cs) g
  ;;

  let iter ~f = iter' ~f true []
end

type modul =
  { name : Symbol.t
  ; hp : wire list
  ; mem : wire list * wire list
  ; out_args : wire list
  ; args : wire list
  ; body : Gates.t
  }
[@@deriving show, eq]
