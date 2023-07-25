open Ograd
open Engine

let lol () =
  let open Value.O in
  let h = 0.0001 in
  let a = Value.create ~label:"a" 2.0 in
  let b = Value.create ~label:"b" (-3.0) in
  let c = Value.create ~label:"c" 10.0 in
  let e = a * b |> Value.set_label "e" in
  let d = e + c |> Value.set_label "d" in
  let f = Value.create ~label:"f" (-2.0) in
  let l = d * f |> Value.set_label "l" in
  let l1 = Value.data l in
  let a = Value.create ~label:"a" 2.0 in
  let b = Value.create ~label:"b" (-3.0) in
  let c = Value.create ~label:"c" (10.0 +. h) in
  let e = a * b |> Value.set_label "e" in
  let d = e + c |> Value.set_label "d" in
  let f = Value.create ~label:"f" (-2.0) in
  let l = d * f |> Value.set_label "l" in
  let l2 = Value.data l in
  let grad = (l2 -. l1) /. h in
  let () = Draw.save_graph "mygraph.dot" l in
  Float.to_string grad |> print_endline

let neuron () =
  let open Value.O in
  let x1 = Value.create ~label:"x1" 2.0 in
  let x2 = Value.create ~label:"x2" 0.0 in
  (* Weights *)
  let w1 = Value.create ~label:"w1" (-3.0) in
  let w2 = Value.create ~label:"w2" 1.0 in
  let b = Value.create ~label:"b" 6.7 in
  let x1w1 = x1 * w1 |> Value.set_label "x1*w1" in
  let x2w2 = x2 * w2 |> Value.set_label "x2*w2" in
  let x1w1x2w2 = x1w1 + x2w2 |> Value.set_label "x1*w1+x2*w2" in
  let n = x1w1x2w2 + b |> Value.set_label "n" in
  Value.to_string n |> print_endline

let basic () =
  let open Value.O in
  let a = Value.create ~label:"a" 2.0 in
  let b = Value.create ~label:"b" (-3.0) in
  let c = Value.create ~label:"c" 10.0 in
  let e = a * b |> Value.set_label "e" in
  let d = e + c |> Value.set_label "d" in
  let f = Value.create ~label:"f" (-2.0) in
  let l = d * f |> Value.set_label "l" in
  print_endline (Value.to_string l);
  print_endline (Value.parents l);
  print_endline (Value.op l)

let () =
  basic ();
  lol ();
  neuron ()
