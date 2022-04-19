val fetch : (Jv.t -> 'a) -> string -> unit -> 'a Fut.t
(* [fetch f uri ()] fetches the JSON data from [uri] applying [f] to
   the data, returning a promise. *)
