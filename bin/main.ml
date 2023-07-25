open Core
open Ograd
open Nn

let () =
  let rounds = 100 in
  let mlp = MLP.create 3 [ 4; 4; 1 ] in
  let xs =
    [
      [ 2.0; 3.0; -1.0 ];
      [ 3.0; -1.0; 0.5 ];
      [ 0.5; 1.0; 1.0 ];
      [ 1.0; 1.0; -1.0 ];
    ]
  in
  let ys = [ 1.0; -1.0; -1.0; 1.0 ] |> List.map ~f:Value.create in
  Train.train ~rounds ~mlp ~xs ~ys
