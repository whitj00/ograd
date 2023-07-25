open Core

module Op = struct
  type t = Empty | Mul | Add | Tanh | Exp | Pow [@@deriving sexp]

  let to_string = function
    | Empty -> ""
    | Mul -> "*"
    | Add -> "+"
    | Tanh -> "tanh"
    | Exp -> "exp"
    | Pow -> "**"
end

let id = ref 0

module Value = struct
  module T = struct
    type t = {
      data : float;
      prev : t list;
      op : Op.t;
      label : string;
      grad : float Ref.t;
      uid : int;
      backward : (unit -> unit) Ref.t;
    }
    [@@deriving fields, sexp]

    let compare t1 t2 = t1.uid - t2.uid
  end

  include T
  include Comparable.Make (T)

  let create ?(label = "") ?(prev = []) ?(op = Op.Empty) data =
    let () = incr id in
    let backward = ref ignore in
    { data; prev; op; label; grad = ref 0.; uid = !id; backward }

  let data t = t.data

  let to_string t =
    [
      "Value(label=";
      t.label;
      ", data=";
      Float.round_decimal t.data ~decimal_digits:4 |> Float.to_string;
      ", grad=";
      Float.round_decimal !(t.grad) ~decimal_digits:4 |> Float.to_string;
      ")";
    ]
    |> String.concat

  let add t1 t2 =
    let node = t1.data +. t2.data in
    let out = create ~prev:[ t1; t2 ] ~op:Add node in
    let backward () =
      t1.grad := !(t1.grad) +. (1.0 *. !(out.grad));
      t2.grad := !(t2.grad) +. (1.0 *. !(out.grad))
    in
    out.backward := backward;
    out

  let mul t1 t2 =
    let node = t1.data *. t2.data in
    let out = create ~prev:[ t1; t2 ] ~op:Mul node in
    let backward () =
      t1.grad := !(t1.grad) +. (t2.data *. !(out.grad));
      t2.grad := !(t2.grad) +. (t1.data *. !(out.grad))
    in
    out.backward := backward;
    out

  let pow t power =
    let label =
      "**" ^ (Float.round_decimal ~decimal_digits:4 power |> Float.to_string)
    in
    let out = create ~op:Pow ~prev:[ t ] ~label (t.data **. power) in
    let backward () =
      t.grad := power *. (t.data **. (power -. 1.)) *. !(out.grad)
    in
    out.backward := backward;
    out

  let tanh t =
    let n = t.data in
    let node = (exp (2. *. n) -. 1.) /. (exp (2. *. n) +. 1.) in
    let out = create ~prev:[ t ] ~op:Tanh node in
    let backward () =
      t.grad := !(t.grad) +. ((1. -. (node **. 2.)) *. !(out.grad))
    in
    out.backward := backward;
    out

  let exp t =
    let x = t.data in
    let exp_x = exp x in
    let out = create ~op:Exp ~prev:[t] exp_x in
    let backward () = t.grad := out.data *. !(out.grad) in
    out.backward := backward;
    out

  let div t1 t2 = mul t1 (pow t2 (-1.))
  let neg t = mul t (create (-1.))
  let sub t1 t2 = add t1 (neg t2)
  let parents t = t.prev |> List.map ~f:to_string |> String.concat ~sep:", "
  let op t = Op.to_string t.op
  let set_label label t = { t with label }

  module O = struct
    let ( + ) = add
    let ( * ) = mul
    let ( ** ) = pow
    let ( / ) = div
    let ( - ) = sub

    let exp = exp
  end
end

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

let backwards t =
  let topo = topo_sort t in
  let grad = Value.grad t in
  grad := 1.;
  List.iter topo ~f:(fun v -> !(Value.backward v) ())
