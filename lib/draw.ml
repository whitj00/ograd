open Engine

module Node = struct
  type t = Value.t

  let compare = Value.compare
  let hash = Hashtbl.hash
  let equal = ( = )
end

module Edge = struct
  type t = String.t

  let compare = String.compare
  let equal = String.equal
  let default = ""
end

module G = Graph.Persistent.Digraph.ConcreteBidirectionalLabeled (Node) (Edge)

let build_graph (leaf : Value.t) =
  let rec build_graph_helper g l =
    let parents = Value.prev l in
    List.fold_left
      (fun g p ->
        let vertex = G.V.create p in
        let g = G.add_vertex g vertex in
        let g = G.add_edge_e g (p, Value.op l, l) in
        List.fold_left build_graph_helper g parents)
      g parents
  in
  let g = G.empty in
  let g = G.add_vertex g leaf in
  build_graph_helper g leaf

module Dot = Graph.Graphviz.Dot (struct
  include G (* use the graph module from above *)

  let edge_attributes (_, e, _) = [ `Label e; `Color 4711 ]
  let default_edge_attributes _ = []
  let get_subgraph _ = None

  let vertex_attributes v =
    let label = Value.to_string v in
    [ `Shape `Box; `Label label ]

  let vertex_name v = Value.uid v |> Int.to_string
  let default_vertex_attributes _ = []
  let graph_attributes _ = [ `Rankdir `LeftToRight ]
end)

let save_graph location leaf =
  let file = open_out_bin location in
  build_graph leaf |> Dot.output_graph file
