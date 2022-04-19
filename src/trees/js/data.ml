open Brr_io

let fetch f uri =
  let open Fut.Syntax in
  let cache = ref None in
  fun () ->
    match !cache with
    | Some v -> Fut.return v
    | None -> (
        let request = Fetch.Request.v Jstr.(v uri) in
        let* data = Fetch.request request in
        match data with
        | Error _ -> failwith "Error handling the data"
        | Ok data ->
            let+ v = Fetch.Response.as_body data |> Fetch.Body.json in
            let ok = Result.get_ok v in
            let v = f ok in
            cache := Some v;
            v)
