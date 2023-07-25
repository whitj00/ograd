open Core
open Nn

let topo_sort (leaf : Value.t) =
  let rec topo_sort_helper acc v =
    let visited, topo = acc in
    match Set.mem visited v with
    | true -> (visited, topo)
    | false ->
        let visited = Set.add visited v in
        let parents = Value.prev v in
        let visited, topo =
          List.fold parents ~init:(visited, topo) ~f:topo_sort_helper
        in
        (visited, v :: topo)
  in
  let _visited, topo = topo_sort_helper (Set.empty (module Value), []) leaf in
  topo

let forward_pass mlp xs ys =
  let open Value.O in
  let ypred = List.map xs ~f:(fun x -> MLP.call' x mlp |> List.hd_exn) in
  let zipped = List.zip_exn ys ypred in
  let losses = List.map zipped ~f:(fun (ygt, yout) -> (yout - ygt) ** 2.) in
  List.fold losses ~init:(Value.create 0.0) ~f:( + ) |> Value.set_label "loss"

let backwards_pass mlp loss =
  MLP.zero_grads mlp;
  let topo = topo_sort loss in
  Value.grad loss := 1.;
  List.iter topo ~f:(fun v -> !(Value.backward v) ())

let update mlp =
  let parameters = MLP.parameters mlp in
  List.iter parameters ~f:(fun p ->
      let current_value = Value.data p in
      let new_value = current_value +. (-0.05 *. !(p.grad)) in
      Value.set_data p new_value)

let full_pass mlp xs ys =
  let loss = forward_pass mlp xs ys in
  let () = backwards_pass mlp loss in
  let () = update mlp in
  printf "loss: %f\n" (Value.data loss)

let train ~rounds ~mlp ~xs ~ys =
  let _ = List.init rounds ~f:(fun _ -> full_pass mlp xs ys) in
  ()
