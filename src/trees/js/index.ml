open Brr
open Brr_io
open Leaflet.Mapbox

let tree_data =
  Data.fetch (fun v -> Jv.get v "features" |> Jv.to_jv_array) "./trees.json"

let stats_data = Data.fetch Tree.stats_of_json "./stats.json"
let default_latlng = Leaflet.LatLng.create ~lat:54.5827 ~lng:(-5.9135)
let default_zoom = 12

(* For good performance we use circle markers -- these are drawn directly to the
   canvas and not rendered as DOM elements... we have a lot of trees to draw ^^' *)


   (* kind : [ `Park | `Street ];
  species_kind : string;
  species : string;
  age : string;
  description : string;
  tree_surround : string;
  vigour : string;
  condition : string;
  diameter : int;
  spread_radius : int; *)

let get_string n jv = (Jstr.to_string @@ Jv.Jstr.get jv n)
let popup jv =
  "<p>Species Kind: " ^ get_string "species_kind" jv
  ^ "</p><p>Species: " ^ get_string "species" jv
  ^ "</p><p>Diameter: " ^ (string_of_int @@ Jv.Int.get jv "diameter")
  ^ "cm</p><p>Height: " ^ (string_of_float @@ Jv.Float.get jv "height")
  ^ "m</p><p>Description: " ^ get_string "description" jv
  ^ "</p><p>Condition: " ^ get_string "condition" jv ^ "</p>"
let on_each_feature ft layer =
  match Jv.find ft "properties" with
  | Some jv ->
      let _jv : Jv.t =
        Jv.call layer "bindPopup" [| Jv.of_string (popup jv) |]
      in
      ()
  | _ -> ()

let color_map jv =
  let s = Jv.to_string @@ Jv.get (Jv.get jv "properties") "species_kind" in
  App.color_map s

let canvas_renderer = Jv.call Leaflet.G.l "canvas" [||]

let circle_marker color =
  Jv.obj
    [|
      ("radius", Jv.of_int 5);
      ("fillColor", Jv.of_string color);
      ("color", Jv.of_string "black");
      ("weight", Jv.of_int 1);
      ("opacity", Jv.of_int 1);
      ("fillOpacity", Jv.of_float 0.8);
      ("renderer", canvas_renderer);
    |]

let point_to_layer jv latlng =
  Jv.call Leaflet.G.l "circleMarker" [| latlng; circle_marker (color_map jv) |]

let map () =
  set_access_token
    "pk.eyJ1IjoicGF0cmljb2ZlcnJpcyIsImEiOiJja3V2a2ZlcmEwZXczMm5xZnRmZWoxOHl5In0.3qTahPB2TaJyK_yRQJuPpQ";
  let map = Map.create "map" in
  let map = Map.set_view ~latlng:default_latlng ~zoom:default_zoom map in
  let map =
    Map.add_layer map
      (`Style (StyleLayer.create "mapbox://styles/mapbox/streets-v11"))
  in
  Map.to_map map

let main () =
  Fut.await (stats_data ()) @@ fun stats -> App.set_stats (fun _ -> Some stats)

let onload el app =
  let ui = Lwd.observe app in
  let on_invalidate _ =
    Console.(log [ str "on invalidate" ]);
    let (_ : int) =
      G.request_animation_frame @@ fun _ ->
      let _ui = Lwd.quick_sample ui in
      ()
    in
    ()
  in
  let on_load _ =
    Console.(log [ str "onload" ]);
    El.append_children el [ Lwd.quick_sample ui ];
    Lwd.set_on_invalidate ui on_invalidate
  in
  Ev.listen Ev.dom_content_loaded on_load (Window.as_target G.window);
  main ()

let setup_map map =
  Fut.await (tree_data ()) @@ fun gj ->
  let layer =
    Jv.call Leaflet.G.l "geoJSON"
      [|
        Jv.of_jv_array gj;
        Jv.obj
          [|
            ("pointToLayer", Jv.repr point_to_layer);
            ("onEachFeature", Jv.repr on_each_feature);
          |];
      |]
  in
  let _jv : Jv.t = Jv.call layer "addTo" [| Leaflet.Map.to_jv map |] in
  ()

let storage = Brr_io.Storage.local G.window
let ni_forest_dark = Jstr.v "ni-forest-dark"

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
  | Some el -> 
    onload el (App.v ());
    setup_map map
  | None -> ()
