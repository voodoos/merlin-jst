type t

open Shape.Sig_component_kind

module Paths = struct
  module T = struct
    type t = Shape.Sig_component_kind.t * Path.t

    let rec length acc = function
      | Path.Pident _ -> acc + 1
      | Pdot (p, _) |  Papply (p, _)
      | Pextra_ty (p, _) -> length (acc + 1) ( p)

    let length (_, p) = length 0 p

    let compare p1 p2 = if p1 == p2 then 0 else length p1 - length p2
  end

  include Set.Make (T)
end

let empty = []

let g = Local_store.s_ref Paths.empty

let add kind path = g := Paths.add (kind, path) !g

let add_type path =
  Format.eprintf "Add type %a\n%!" Path.print path;
  g := Paths.add (Type, path) !g
let add_module path = g := Paths.add (Module, path) !g

let canonical_paths : Paths.t Path.Map.t ref = Local_store.s_ref Path.Map.empty

let graph = Short_paths_graph.Graph.empty
