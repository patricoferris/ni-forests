open Brr

module G : sig
  val l : Jv.t
end

module Layer : sig
  type t

  include Jv.CONV with type t := t
end

module LatLng : sig
  type t = Jv.t

  include Jv.CONV with type t := t

  val create : lat:float -> lng:float -> t
end

module TileLayer : sig
  type opts

  val opts_to_jv : opts -> Jv.t

  val opts :
    ?min_zoom:int ->
    ?max_zoom:int ->
    ?subdomains:string array ->
    ?error_title_url:string ->
    ?zoom_offset:int ->
    ?tms:bool ->
    ?zoom_reverse:bool ->
    ?detect_retina:bool ->
    ?cross_origin:bool ->
    unit ->
    opts

  type t

  include Jv.CONV with type t := t

  val create : ?opts:opts -> string -> t
  val set_url : ?no_redraw:bool -> t -> string -> unit
end

module Map : sig
  type opts

  val opts_to_jv : opts -> Jv.t

  val opts :
    ?prefer_canvas:bool ->
    ?attribution_control:bool ->
    ?zoom_control:bool ->
    ?close_popup_on_click:bool ->
    ?zoom_snap:int ->
    ?zoom_delta:int ->
    ?track_resize:bool ->
    ?box_zoom:bool ->
    ?double_click_zoom:bool ->
    ?dragging:bool ->
    ?center:Jv.t ->
    ?zoom:int ->
    ?min_zoom:int ->
    ?max_zoom:int ->
    unit ->
    opts

  type t

  include Jv.CONV with type t := t

  val create : ?opts:opts -> string -> t
  val create_with_el : ?opts:opts -> El.t -> t
  val set_view : latlng:LatLng.t -> zoom:int -> t -> t
  val add_layer : t -> [ `Layer of Layer.t | `Tile of TileLayer.t ] -> t
end

module Control : sig
  type t 
  type position = TopLeft | TopRight | BottomLeft | BottomRight

  include Jv.CONV with type t := t

  val create : ?position:position -> on_add:(Map.t -> El.t) -> update:(Jv.t -> unit) -> unit -> t
  val add_to : map:Map.t -> t -> t
  val remove : t -> unit
end