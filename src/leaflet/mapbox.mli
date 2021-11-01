open Brr

module StyleLayer : sig
  type t

  include Jv.CONV with type t := t

  val create : ?opts:L.TileLayer.opts -> string -> t
  (** Extend {! L.TileLayer} with a URL from mapbox *)
end

val set_access_token : string -> unit

module Map : sig
  type opts

  val opts_to_jv : opts -> Jv.t
  val opts : ?base_opts:L.Map.opts -> ?access_token:string -> unit -> opts

  type t

  include Jv.CONV with type t := t

  val create : ?opts:opts -> ?url:string -> string -> t
  val create_with_el : ?opts:opts -> ?url:string -> El.t -> t
  val to_map : t -> L.Map.t
  val set_view : latlng:L.LatLng.t -> zoom:int -> t -> t
  val add_layer :
    t ->
    [ `Layer of L.Layer.t | `Tile of L.TileLayer.t | `Style of StyleLayer.t ] ->
    t
end
