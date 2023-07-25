open Core

module Neuron = struct
  type t = { w : Value.t List.t; b : Value.t } [@@deriving fields, sexp]

  let create nin =
    {
      w = List.init nin ~f:(fun _ -> Value.create (Random.float_range (-1.) 1.));
      b = Value.create (Random.float_range (-1.) 1.);
    }
    [@@deriving fields]

  let call x t =
    let open Value.O in
    let zipped = List.zip_exn t.w x in
    let act =
      List.fold zipped ~init:t.b ~f:(fun acc (wi, xi) -> acc + (wi * xi))
    in
    tanh act

  let call' x t = call (List.map x ~f:Value.create) t
  let parameters t = t.w @ [ t.b ]
end

module Layer = struct
  type t = { neurons : Neuron.t List.t } [@@deriving fields]

  let create nin nout =
    { neurons = List.init nout ~f:(fun _ -> Neuron.create nin) }

  let call x t = List.map t.neurons ~f:(fun neuron -> Neuron.call x neuron)
  let call' x t = call (List.map x ~f:Value.create) t
  let parameters t = List.map t.neurons ~f:Neuron.parameters |> List.concat
end

module MLP = struct
  type t = { layers : Layer.t List.t } [@@deriving fields]

  let create nin nouts =
    let sz = [ nin ] @ nouts in
    {
      layers =
        List.init (List.length nouts) ~f:(fun i ->
            let sz_i = List.nth_exn sz i in
            let sz_i_plus_one = List.nth_exn sz (i + 1) in
            Layer.create sz_i sz_i_plus_one);
    }

  let call x t =
    List.fold t.layers ~init:x ~f:(fun acc layer -> Layer.call acc layer)

  let call' x t = call (List.map x ~f:Value.create) t
  let parameters t = List.map t.layers ~f:Layer.parameters |> List.concat
  let zero_grads t = parameters t |> List.iter ~f:Value.zero_grad
end
