module Brr_parser = struct
  type t = Jv.t

  let catch_err f v = try Ok (f v) with _ -> Error (`Msg "Brr Error")

  let find t s =
    let rec loop jv = function
      | [] -> jv
      | x :: xs -> loop (Option.bind jv (fun jv -> Jv.find jv x)) xs
    in
    loop (Some t) s

  let to_string t = catch_err Jv.to_string t
  let string = Jv.of_string
  let to_float t = catch_err Jv.to_float t
  let float = Jv.of_float
  let to_int t = catch_err Jv.to_int t
  let int = Jv.of_int
  let to_list f t = catch_err (Jv.to_list f) t
  let list f t = Jv.of_list f t
  let to_array f t = catch_err (Jv.to_array f) t
  let array f t = Jv.of_array f t
  let obj = Jv.get Jv.global "Object"

  let keys jv =
    let keys_method = Jv.get obj "keys" in
    Jv.apply keys_method [| jv |] |> Jv.to_jstr_list |> List.map Jstr.to_string

  let to_obj t =
    let keys = keys t in
    Ok (List.map (fun key -> (key, Jv.get t key)) keys)

  let obj arr = Jv.obj (Array.of_list arr)
  let null = Jv.null
  let is_null = Jv.is_null
end

include Belfast_tree.Make_geojson (Brr_parser)
