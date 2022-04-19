open Eio.Std

module Ezjsonm_parser = struct
  type t = Ezjsonm.value

  let catch_err f v =
    try Ok (f v) with Ezjsonm.Parse_error (_, s) -> Error (`Msg s)

  let find = Ezjsonm.find_opt
  let to_string t = catch_err Ezjsonm.get_string t
  let string = Ezjsonm.string
  let to_float t = catch_err Ezjsonm.get_float t
  let float = Ezjsonm.float
  let to_int t = catch_err Ezjsonm.get_int t
  let int = Ezjsonm.int
  let to_list f t = catch_err (Ezjsonm.get_list f) t
  let list f t = Ezjsonm.list f t
  let to_array f t = Result.map Array.of_list @@ to_list f t
  let array f t = list f (Array.to_list t)
  let to_obj t = catch_err Ezjsonm.get_dict t
  let obj = Ezjsonm.dict
  let null = `Null
  let is_null = function `Null -> true | _ -> false
end

module Tree = Belfast_tree.Make_geojson (Ezjsonm_parser)

let remove_quotes s =
  match s.[0] with '"' -> String.sub s 1 (String.length s - 2) | _ -> s

let entries_to_trees es =
  Seq.map (String.split_on_char ',') es
  |> Seq.map (List.map remove_quotes)
  |> Seq.map Belfast_tree.of_csv_line

let stats trees = Belfast_tree.stats trees |> Tree.stats_to_json

let () =
  let input_csv = Sys.argv.(1) in
  let output_json = Sys.argv.(2) in
  let stats_json = Sys.argv.(3) in
  Eio_main.run @@ fun stdenv ->
  Switch.run @@ fun _sw ->
  Eio.Dir.with_lines (Eio.Stdenv.fs stdenv) input_csv @@ fun lines ->
  let _header, entries =
    (Seq.take 1 lines, Seq.drop 1 lines |> entries_to_trees)
  in
  let trees = List.of_seq entries in
  let list = Tree.trees_to_geojson trees in
  Eio.Dir.save ~create:(`Exclusive 0o600) (Eio.Stdenv.fs stdenv) output_json
    (Tree.G.to_json list |> Ezjsonm.value_to_string);
  Eio.Dir.save ~create:(`Exclusive 0o600) (Eio.Stdenv.fs stdenv) stats_json
    (Ezjsonm.value_to_string @@ stats trees)
