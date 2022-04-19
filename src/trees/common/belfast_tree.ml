(* This module is specific to the CSV format of the Belfast Trees dataset. *)

type t = {
  kind : [ `Park | `Street ];
  species_kind : string;
  species : string;
  age : string;
  description : string;
  tree_surround : string;
  vigour : string;
  condition : string;
  diameter : int;
  spread_radius : int;
  (* Don't want tree locationx or locationy *)
  lng : float;
  lat : float;
  tag : int option;
  height : float;
}

let kind_of_string = function
  | "\"ParkTree\"" | "ParkTree" -> `Park
  | "\"StreetTree\"" | "StreetTree" -> `Street
  | s -> failwith ("Unknown tree kind: " ^ s)

let kind_to_string = function `Park -> "ParkTree" | `Street -> "StreetTree"

let check_length line =
  match List.length line with
  | 16 -> ()
  | i -> failwith ("Expected 16 entries, but got " ^ string_of_int i)

let int_of_string_err v =
  try int_of_string v
  with _ -> failwith ("Failed turning " ^ v ^ " into integer")

let of_csv_line line =
  check_length line;
  {
    kind = List.nth line 0 |> kind_of_string;
    species_kind = List.nth line 1;
    species = List.nth line 2;
    age = List.nth line 3;
    description = List.nth line 4;
    tree_surround = List.nth line 5;
    vigour = List.nth line 6;
    condition = List.nth line 7;
    diameter = List.nth line 8 |> int_of_string_err;
    spread_radius = List.nth line 9 |> int_of_string_err;
    (* Don't want tree locationx or locationy *)
    lng = List.nth line 12 |> float_of_string;
    lat = List.nth line 13 |> float_of_string;
    tag = List.nth line 14 |> int_of_string_opt;
    height = List.nth line 15 |> float_of_string;
  }

type stats = { species_kind_percentage : (string * (int * float)) List.t }

let stats trees =
  let module H = Hashtbl.Make (String) in
  let tbl = H.create 42 in
  let add_tree kind =
    match H.find_opt tbl kind with
    | Some (i, v) -> H.replace tbl kind ((i + 1), v)
    | _ -> H.add tbl kind (1, 0.)
  in
  List.iter (fun v -> add_tree v.species_kind) trees;
  let total = List.length trees |> float_of_int in
  let entries = H.to_seq tbl |> List.of_seq |> List.map (fun (n, (i, v)) -> (n, (i, 100. *. float_of_int i /. total))) in
  {
    species_kind_percentage = List.sort (fun (_m, (v1, _)) (_n, (v2, _)) -> Int.neg (Int.compare v1 v2)) entries;
  }

module Make_geojson (J : Geojson__Geojson_intf.Json) = struct
  module G = Geojson.Make (J)

  let tree_to_geojson t =
    let point =
      G.Geometry.Position.v ~long:t.lng ~lat:t.lat () |> G.Geometry.Point.v
    in
    let properties =
      J.obj
        [
          ("kind", J.string (kind_to_string t.kind));
          ("species_kind", J.string t.species_kind);
          ("species", J.string t.species);
          ("age", J.string t.age);
          ("description", J.string t.description);
          ("tree_surround", J.string t.tree_surround);
          ("vigour", J.string t.vigour);
          ("condition", J.string t.condition);
          ("diameter", J.float (float_of_int t.diameter));
          ("spread_radius", J.float (float_of_int t.spread_radius));
          ( "tag",
            match t.tag with
            | Some tag -> J.float (float_of_int tag)
            | _ -> J.null );
          ("height", J.float t.height);
        ]
    in
    G.Feature.v ~properties (G.Geometry.Point point)

  let trees_to_geojson ts =
    G.v
    @@ G.FeatureCollection
         (G.Feature.Collection.v (List.map tree_to_geojson ts))

  let stats_to_json { species_kind_percentage } =
    J.obj
      [
        ( "species_kind_percentage",
          J.obj
            (List.map
               (fun (k, (v, f)) -> (k, J.obj [ "count", J.float @@ float_of_int v; "percent", J.float f ]))
               species_kind_percentage) );
      ]

  let or_error f v = match f v with Ok v -> v | Error (`Msg m) -> failwith m

  let count_and_percent jv =
    match J.find jv [ "count" ], J.find jv [ "percent" ] with
      | Some c, Some p ->
        (or_error J.to_int c, or_error J.to_float p)
      | _ -> failwith "count and percent failed"

  let stats_of_json json =
    match J.find json [ "species_kind_percentage" ] with
    | None -> failwith "Failed to parse stats!"
    | Some nvs -> (
        match J.to_obj nvs with
        | Ok obj ->
            List.map (fun (n, v) -> (n, count_and_percent v)) obj
            |> fun species_kind_percentage -> { species_kind_percentage }
        | Error (`Msg m) -> failwith m)
end
