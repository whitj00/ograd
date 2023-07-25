open Core

module Op = struct
  type t = Empty | Mul | Add [@@deriving sexp]

  let to_string = function Empty -> "" | Mul -> "*" | Add -> "+"
end

let id = ref 0

module Value = struct
  module T = struct
  type t = {
    data : float;
    prev : t list;
    op : Op.t;
    label : string;
    grad : float;
    uid : int;
  }
  [@@deriving fields, sexp]
  let compare t1 t2 = t1.uid - t2.uid
  end
  include T
  include Comparable.Make (T)

  let create ?(label = "") ?(prev = []) ?(op = Op.Empty) data =
    let () = incr id in
    { data; prev; op; label; grad = 0.; uid = !id }

  let data t = t.data
  let to_string t = "Value(data=" ^ Float.to_string t.data ^ ")"
  let add t1 t2 = t1.data +. t2.data |> create ~prev:[ t1; t2 ] ~op:Add
  let mul t1 t2 = t1.data *. t2.data |> create ~prev:[ t1; t2 ] ~op:Mul
  let parents t = t.prev |> List.map ~f:to_string |> String.concat ~sep:", "
  let op t = Op.to_string t.op
  let set_label label t = { t with label }

  module O = struct
    let ( + ) = add
    let ( * ) = mul
  end
end
