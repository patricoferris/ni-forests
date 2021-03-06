open Brr
open Brr_lwd
open Brr_io
open Let_syntax
open Leaflet.Mapbox

(* ~~~ Helpers ~~~ *)
let colours = [ "#edf8fb"; "#b2e2e2"; "#66c2a4"; "#2ca25f"; "#006d2c"]

let default_latlng = Leaflet.LatLng.create ~lat:54.6627 ~lng:(-6.7135)
let default_zoom = 8

let set_inner_html el html =
  let jv = El.to_jv el in
  Jv.set jv "innerHTML" (Jv.of_string html);
  Jv.set jv "scrollTop" (Jv.of_int 0)

let set_classes el cls =
  let set_cl cl = El.set_class (Jstr.v cl) true el in
  List.iter set_cl cls

let ( / ) s t = Jv.get s t

(* ~~~ Counties GeoJSON ~~~ *)
module County = struct
  type internal = {
    county : Jstr.t;
    total : int;
    percentage : float;
    woodlands : Jv.t;
  }[@@deriving brr]
  type t = Jv.t
end

let counties : County.t list =
  let jv = Jv.get Jv.global "__NI_COUNTIES_DATA__" in
  Jv.to_jv_array jv |> Array.to_list


let fetch_data uri =
  let open Fut.Syntax in
  let cache = ref None in
  fun () ->
  match !cache with
    | Some v -> Fut.return v
    | None ->
      (* Big Json :) *)
      let request = Fetch.Request.v Jstr.(v uri) in
      let* data = Fetch.request request in
      match data with
       | Error _ -> failwith "Error handling the peatland data"
       | Ok data ->
      let+ v = Fetch.Response.as_body data |> Fetch.Body.json in
      let ok = Result.get_ok v in
      let v = Jv.get ok "features" |> Jv.to_jv_array in
      cache := Some v;
      v

let peatland_data = fetch_data "./ni-peatland.json"
let seagrass_data = fetch_data "./ni-intertidal-seagrass.geojson"

let amount_to_colour =
  let minimum = List.fold_left min Float.max_float (List.map County.percentage counties) in
  let maximum = List.fold_left max Float.min_float (List.map County.percentage counties) in
  let step = (maximum -. minimum) /. float_of_int (List.length colours) in
  fun f ->
    let rec aux = function
      | i -> if f >= minimum +. (step *. (i -. 1.)) && f <= minimum +. (step *. i) then i else aux (i +. 1.)
    in
    let i = int_of_float @@ aux 0. in
    match List.nth_opt colours i with
      | Some s -> s
      | None -> List.nth colours (List.length colours - 1)

let counties_json =
  let jv = Jv.get Jv.global "__NI_COUNTIES__" in
  let arr = Jv.get jv "features" |> Jv.to_jv_array in
  let add_data jv =
    let props = Jv.get jv "properties" in
    let name = Jv.Jstr.get props "CountyName" |> Jstr.to_string in
    match List.find_opt (fun jv -> String.uppercase_ascii @@ Jstr.to_string (County.county jv) = name) counties with
      | Some c ->
        let props = Jv.get jv "properties" in
        Jv.set props "data" c
      | None -> ()
  in
  Array.iter add_data arr;
  arr

(* ~~~ Actions ~~~
   Each action is for a block of text and is so transformation
   on the map. Each action also records how to undo itself.
*)

(* Overview of the counties and their woodland/forest cover *)
let counties_and_forest_cover =
  let geojson = ref None in
  let data_div = ref None in
  let on_add _ =
    let div = El.div [] in
    set_classes div [ "info" ];
    data_div := Some div;
    div
  in
  let update props =
    let data = Jv.get props "data" in
    let county = County.county data |> Jstr.to_string in
    let total_hectare = "<p><b>Total</b>: " ^ string_of_int (County.total data) ^ " ha</p>" in
    let percentage = "<p><b>Percentage</b>: " ^ string_of_float (County.percentage data) ^ "%</p>" in
    Option.iter (fun v -> set_inner_html v ("<h4>County: " ^ county ^ "</h4>" ^ total_hectare ^ percentage)) !data_div
  in
  let info = Leaflet.Control.create ~on_add ~update () in
  let hover e =
    let layer = Ev.target e |> Ev.target_to_jv in
    let props = Jv.get layer "feature" |> fun jv -> Jv.get jv "properties" in
    update props;
    Jv.call layer "setStyle" Jv.[| obj [| "weight", Jv.of_int 4; "fillOpacity", Jv.of_float 0.9 |]|]
  in
  let reset e =
    let layer = Ev.target e |> Ev.target_to_jv in
    Option.iter (fun v -> ignore @@ Jv.call v "resetStyle" [| layer |]) !geojson
  in
  let on_each_feature _feature layer =
    Jv.call layer "on" Jv.[| obj [| "mouseover", Jv.repr hover; "mouseout", Jv.repr reset |] |]
  in
  let style ft =
    let props = Jv.get ft "properties" in
    let county = Jv.get props "data" in
    let perc = County.percentage county in
    Jv.obj [|
      "fillColor", Jv.of_string @@ amount_to_colour perc;
      "color", Jv.of_string "black";
      "weight", Jv.of_int 1;
      "fillOpacity", Jv.of_float 0.7
    |]
  in
  fun map ->
  let m = Jv.call Leaflet.G.l "geoJson" [| Jv.of_jv_array counties_json; Jv.obj [| "style", Jv.repr style; "onEachFeature", Jv.repr on_each_feature |] |] in
  geojson := Some m;
  let control = Leaflet.Control.add_to ~map info in
  Jv.call m "addTo" [| Leaflet.Map.to_jv map |], (fun () -> Leaflet.Control.remove control)

(* Zooming to function *)

type poi = Default | Cavehill | Strangford_lough
let zoom_to ?(zoom = default_zoom) =
  let poi_to_latlng = function
    | Cavehill -> Leaflet.LatLng.(create ~lat:54.6558683 ~lng:(-5.9521483) |> to_jv)
    | Strangford_lough -> Leaflet.LatLng.(create ~lat:54.5591563 ~lng:(-5.6714574) |> to_jv)
    | Default -> Leaflet.LatLng.(default_latlng |> to_jv)
  in
  fun poi map ->
  let _ = Jv.call (Leaflet.Map.to_jv map) "flyTo" [| poi_to_latlng poi ; Jv.of_int zoom |] in
  ()

(* Peatland *)
let make_layer ?(color = "red") ?(opacity = 0.7) data map =
  let geojson = ref None in
  let style _ft =
    Jv.obj [|
      "fillColor", Jv.of_string color;
      "color", Jv.of_string "black";
      "weight", Jv.of_int 1;
      "fillOpacity", Jv.of_float opacity
    |]
  in
  let m = Jv.call Leaflet.G.l "geoJson" [| Jv.of_jv_array data; Jv.obj [| "style", Jv.repr style; |] |] in
  geojson := Some m;
  Jv.call m "addTo" [| Leaflet.Map.to_jv map |]


let action =
  let reset_ref = ref (fun () -> ()) in
  let reset () =
    !reset_ref ()
  in
  fun map i ->
  let satellite_layer = StyleLayer.create "mapbox://styles/mapbox/satellite-v9" in
  match i with
  | 0 -> reset ()
  | 1 -> reset ();
    let layer, reset_control = counties_and_forest_cover map in
    let reset_1 () =
      let _ : Jv.t = Jv.call layer "remove" [||] in
      reset_control ()
    in
    reset_ref := reset_1
  | 2 ->
    reset ()
  | 3 -> reset ();
    let _map = Map.add_layer (Map.of_jv @@ Leaflet.Map.to_jv map) (`Style satellite_layer) in
    let reset_3 () =
      let _ = Jv.call (Leaflet.Map.to_jv map) "removeLayer" [| StyleLayer.to_jv satellite_layer |] in
      zoom_to Default map
    in
    reset_ref := reset_3;
    zoom_to ~zoom:14 Cavehill map
  | 4 ->
    reset ();
    (* Apply the peatland data a little later than the reset *)
    let layer = ref None in
    let id : int = Brr.G.set_timeout ~ms:2000 @@ fun () -> Fut.await (peatland_data ()) (fun v -> layer := Some (make_layer v map)) in
    let reset_4 () =
      Brr.G.stop_timer id;
      Option.iter (fun layer -> let _ = Jv.call (Leaflet.Map.to_jv map) "removeLayer" [| layer |] in ()) !layer;
      zoom_to Default map
    in
    reset_ref := reset_4
  | 5 ->
    reset ();
    let layers = ref [ StyleLayer.to_jv satellite_layer ] in
    let _map = Map.add_layer (Map.of_jv @@ Leaflet.Map.to_jv map) (`Style satellite_layer) in
    zoom_to ~zoom:13 Strangford_lough map;
    let id : int = Brr.G.set_timeout ~ms:2000 @@ fun () -> Fut.await (seagrass_data ()) (fun v -> layers := (make_layer ~color:"green" ~opacity:0.3 v map) :: !layers) in
    let reset_5 () =
      Brr.G.stop_timer id;
      let _ = List.iter (fun layer -> ignore @@ Jv.call (Leaflet.Map.to_jv map) "removeLayer" [| layer |]) !layers in
      zoom_to Default map
    in
    reset_ref := reset_5
  | 6 -> reset ()
  | _ -> ()

(* ~~~ Text Viewer ~~~
   The text viewer holds the state (index) which drives
   everything else. Using Lwd and Brr I think this is
   quite neat for small amounts of reactiveness.

   All of the text is stored in ./text as markdown files.
   Each is converted using [omd] and the [cruched] into
   an OCaml module. *)

module TextViewer = struct

  let sections = List.length Text.file_list

  let read_file i =
    let s = List.nth Text.file_list i in
    match Text.read s with
      | Some html -> html
      | None -> "<h1>Failed to read HTML via ocaml-crunch, contact Patrick!</h1>"

  let make map =
    let idx, set_idx = use_state 0 in
    let buttons =
      let base_classes = [ "bg-gray-300"; "hover:bg-gray-400"; "py-2"; "px-4"; "mt-2"] in
      let inc = El.(button [ txt' "Next" ]) in
      Ev.(listen click (fun _ -> set_idx (fun i -> abs (i + 1) mod sections)) El.(as_target inc));
      let dec = El.(button [ txt' "Previous" ]) in
      Ev.(listen click (fun _ -> set_idx (fun i ->  (max 0 (i - 1)) mod sections)) El.(as_target dec));
      set_classes dec ("rounded-l" :: base_classes);
      set_classes inc ("rounded-r" :: base_classes);
      let div = El.(div [ dec; inc] ) in
      set_classes div [ "inline-flex" ];
      div
    in
    let block =
      let ele = El.(div []) in
      set_classes ele [ "prose"; "overflow-x-scroll"; "dark:prose-light" ];
      El.set_inline_style Jstr.(v "height") Jstr.(v "550px") ele;
      ele
    in
    let* counter =
      let text =
        let+ idx = Lwd.get idx in
        El.txt' (string_of_int (idx + 1) ^ "/" ^ string_of_int sections)
      in
      let+ p = Elwd.p [ `R text ] in
      set_classes p [ "dark:text-white" ];
      p
    in
    let* () = Lwd.map ~f:(fun idx -> set_inner_html block (read_file idx)) @@ Lwd.get idx in
    let+ () = Lwd.map ~f:(action map) @@ Lwd.get idx in
    [ counter; block; buttons; ]
end

let map () =
  set_access_token
    "pk.eyJ1IjoicGF0cmljb2ZlcnJpcyIsImEiOiJja3V2a2ZlcmEwZXczMm5xZnRmZWoxOHl5In0.3qTahPB2TaJyK_yRQJuPpQ";
  let map = Map.create "map" in
  let map = Map.set_view ~latlng:default_latlng ~zoom:default_zoom map |> fun map ->
      Map.add_layer map
        (`Style (StyleLayer.create "mapbox://styles/mapbox/streets-v11"))
  in
  Map.to_map map

let storage = Brr_io.Storage.local G.window
let ni_forest_dark = Jstr.v "ni-forest-dark"

(* Dark mode *)
let dark_mode =
  fun () ->
    let toggle ?(toggle = true) () =
      let dark = match Brr_io.Storage.get_item storage ni_forest_dark with Some v -> Jstr.to_string v = "true" | _ -> false in
      El.set_class (Jstr.v "dark") (if toggle then not dark else dark) (Jv.get (Document.to_jv G.document) "documentElement" |> El.of_jv);
      ignore @@ Brr_io.Storage.set_item storage ni_forest_dark Jstr.(v @@ string_of_bool @@ (if toggle then not dark else dark))
    in
    toggle ~toggle:false ();
    match Document.find_el_by_id G.document (Jstr.v "toggle") with
      | Some btn -> Ev.(listen click (fun _ -> toggle ()) @@ El.as_target btn)
      | _ -> failwith "Errrr"

let () =
  let map = map () in
  dark_mode ();
  match Document.find_el_by_id G.document (Jstr.v "main") with
    | Some el -> onload el @@ TextViewer.make map
    | None -> failwith "Can't find main element"
