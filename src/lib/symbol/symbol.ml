open Core
module Table = Int.Table
module Map = Int.Map
module Set = Int.Set

type t = int [@@deriving show, eq]

let sym_to_name, name_to_sym = Table.create (), String.Table.create ()

let get_sym (data : string) : t =
  match String.Table.find name_to_sym data with
  | Some sym -> sym
  | None ->
    let key = Table.length sym_to_name in
    Table.add_exn sym_to_name ~key ~data;
    String.Table.add_exn name_to_sym ~key:data ~data:key;
    key
;;

let nonce () : t =
  let key = Table.length sym_to_name in
  Table.add_exn sym_to_name ~key ~data:("@" ^ Int.to_string key);
  key
;;

let name = Table.find_exn sym_to_name
