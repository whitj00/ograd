open Core

(* Dumb way to create unique IDs*)
let id = ref 0

let get_id () = incr id; !id

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

module T = struct
  type t = {
    data : float Ref.t;
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
  { data = ref data; prev; op; label; grad = ref 0.; uid = get_id (); backward = ref ignore }

let data t = !(t.data)
let set_data t v = t.data := v
let zero_grad t = t.grad := 0.0

let to_string t =
  let data = data t in
  [
    "Value(label=";
    label t;
    ", data=";
    sprintf "%.4f\n" data;
    ", grad=";
    Float.round_decimal !(t.grad) ~decimal_digits:4 |> Float.to_string;
    ")";
  ]
  |> String.concat

let add t1 t2 =
  let node = data t1 +. data t2 in
  let out = create ~prev:[ t1; t2 ] ~op:Add node in
  let backward () =
    t1.grad := !(t1.grad) +. (1.0 *. !(out.grad));
    t2.grad := !(t2.grad) +. (1.0 *. !(out.grad))
  in
  out.backward := backward;
  out

let mul t1 t2 =
  let node = data t1 *. data t2 in
  let out = create ~prev:[ t1; t2 ] ~op:Mul node in
  let backward () =
    t1.grad := !(t1.grad) +. (data t2 *. !(out.grad));
    t2.grad := !(t2.grad) +. (data t1 *. !(out.grad))
  in
  out.backward := backward;
  out

let pow t power =
  let label =
    "**" ^ (Float.round_decimal ~decimal_digits:4 power |> Float.to_string)
  in
  let out = create ~op:Pow ~prev:[ t ] ~label (data t **. power) in
  let backward () =
    t.grad := power *. (data t **. (power -. 1.)) *. !(out.grad)
  in
  out.backward := backward;
  out

let tanh t =
  let n = data t in
  let node = (exp (2. *. n) -. 1.) /. (exp (2. *. n) +. 1.) in
  let out = create ~prev:[ t ] ~op:Tanh node in
  let backward () =
    t.grad := !(t.grad) +. ((1. -. (node **. 2.)) *. !(out.grad))
  in
  out.backward := backward;
  out

let exp t =
  let x = data t in
  let exp_x = exp x in
  let out = create ~op:Exp ~prev:[ t ] exp_x in
  let backward () = t.grad := data out *. !(out.grad) in
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
  let tanh = tanh
end
