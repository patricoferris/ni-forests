open Brr

module G = struct
  let l = Jv.get (Window.to_jv G.window) "L"
  let mapbox = Jv.get l "mapbox"
end

let set_access_token s = Jv.set G.mapbox "accessToken" (Jv.of_string s)

module StyleLayer = struct
  type t = Jv.t

  include (Jv.Id : Jv.CONV with type t := t)

  let create ?opts url =
    let o =
      match opts with Some v -> L.TileLayer.opts_to_jv v | None -> Jv.obj [||]
    in
    Jv.call G.mapbox "styleLayer" [| Jv.of_string url; o |]
end

module Map = struct
  type opts = Jv.t

  let opts_to_jv opts = Jv.Id.to_jv opts

  let opts ?base_opts ?access_token () =
    let o =
      match base_opts with Some v -> L.Map.opts_to_jv v | None -> Jv.obj [||]
    in
    Jv.Jstr.set_if_some o "accessToken" (Option.map Jstr.v access_token);
    o

  type t = Jv.t

  include (Jv.Id : Jv.CONV with type t := t)

  let create ?(opts = Jv.undefined) ?url el_id =
    Jv.call G.mapbox "map"
      [|
        Jv.of_string el_id;
        Jv.of_option ~none:Jv.undefined Jv.of_string url;
        opts;
      |]

  let create_with_el ?(opts = Jv.undefined) ?url el =
    Jv.call G.mapbox "map"
      [| El.to_jv el; Jv.of_option ~none:Jv.undefined Jv.of_string url; opts |]

  let to_map t = to_jv t |> L.Map.of_jv

  let set_view ~latlng ~zoom t = 
    to_map t 
    |> L.Map.set_view ~latlng ~zoom 
    |> L.Map.to_jv
    |> of_jv

  let add_layer t v =
    match v with
    | (`Layer _ | `Tile _) as v ->
        let map = to_jv t |> L.Map.of_jv in
        L.Map.add_layer map v |> L.Map.to_jv |> of_jv
    | `Style s -> Jv.call t "addLayer" [| StyleLayer.to_jv s |]
end
