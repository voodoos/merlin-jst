type t

open Shape.Sig_component_kind

module Paths = struct
  module T = struct
    type t = Shape.Sig_component_kind.t * Path.t

    let rec length acc = function
      | _, Path.Pident _ -> acc + 1
      | _, Pdot (p, _) | _, Papply (p, _) -> length (acc + 1) (Module, p)
      | _, Pextra_ty (p, Pcstr_ty _) -> length (acc + 1) (Type, p)
      | _, Pextra_ty (p, Pext_ty) -> length (acc + 1) (Extension_constructor, p)

    let compare p1 p2 = if p1 == p2 then 0 else length 0 p1 - length 0 p2
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
