open Brr
open Brr_lwd

let use_state a =
  let a = Lwd.var a in
  (a, fun f -> Lwd.set a (f (Lwd.peek a)))

let stats, set_stats = use_state None

let color_map = function
  | "Oak" -> "#d55e00"
  | "Lime" -> "#cc79a7"
  | "Maple" -> "#0072b2"
  | "Ash" -> "#f0e442"
  | "Pine" -> "#009e73"
  | _ -> "grey"

let v () =
  let td_at = [ At.class' (Jstr.v "border"); At.class' (Jstr.v "px-8"); At.class' (Jstr.v "py-4") ] in
  let f = function
    | Some stats ->
        let species = stats.Belfast_tree.species_kind_percentage in
        let color_td s =
          let el = El.td ~at:(td_at) [] in
          Jv.set (El.to_jv el) "style" (Jv.of_string ("background-color: " ^ color_map s));
          el
        in
        let items =
          Lwd.return @@
          Lwd_seq.of_list @@
          List.map
            (fun (n, (i, p)) -> El.tr [ El.td ~at:td_at [ El.txt' n ]; El.td ~at:td_at [ El.txt' (string_of_int i) ]; El.td ~at:td_at [ El.txt' (string_of_float p) ]; color_td n ])
            species
        in
        items
    | None -> Lwd.return @@ Lwd_seq.of_list @@ []
  in
  Elwd.table ~at:[ `P (At.class' (Jstr.v "w-full")) ] [ 
    `P (El.thead [ El.tr ~at:[ At.class' (Jstr.v "uppercase") ] [ El.td ~at:td_at [ El.txt' "Species" ]; El.td ~at:td_at [ El.txt' "Count" ]; El.td ~at:td_at [ El.txt' "Percentage" ]; El.td ~at:td_at [ El.txt' "Colour" ] ]]);
    `R (Elwd.tbody [ `S (Lwd.bind ~f (Lwd.get stats)) ])
  ]
