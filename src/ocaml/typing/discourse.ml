(*

We call U the set of all paths used directly in a file:

- 1. Any path occurring in the file is in U. For example, List.map occurring in
     the file will add both List.map and List to U.
- 2. All paths for definitions in the current file are in U. So if module M = …
     occurs in the file then M is in U.
- 3. All paths for things “defined” using include or open in the current file
     are in U. It is possible that all of these would end up in D anyway via
     other rules, but it's not entirely obvious so I've included this rule here
     just to make sure they do.
- Note that constructors or fields only used via type-based disambiguation are
  not in U.

We call D the domain of discourse:

- 1. The paths of all the predefined types that are intended for direct use by
     users, like int, are in D.
- 2. If a path is in U then it is also in D.
- 3. If a module path is in U then all the paths of its subcomponents are in D.
- 4. If a value path is in U and its value description was written by a user -
     as opposed to being inferred - then the paths used in that description are
     in D.
- 5. If a module path is in U and its module description was written by a user -
     as opposed to being inferred - then the paths used in that description are
     in D, excluding those paths that only appear inside of a sig…end.
- 6. If a type path is in U then any paths used in its equation or
     representation are in D.
- 7. If a constructor or record field is in U then any paths used in its type
     are in D.
- 8. If a module type path is in U then any paths used in its definition are in
     D, excluding those paths that only appear inside of a sig…end.
- 9. If a class path is in U and its class description was written by a user -
     as opposed to being inferred - then of any paths used in that description
     are in D.
- 10. If a class type path is in U then any paths used in its definition are in
      D.
- 11. If a path is in D and it includes another module path within it, then that
      module path is also in D.
- 12. If a module path m in D - note D not U - is a module alias with target n
      and another path p in D includes n within it, then the path obtained by
      substituting the m for n in p is also in D.
*)

type t

open Shape.Sig_component_kind

module Paths = struct
  module T = struct
    type t = Shape.Sig_component_kind.t * Path.t

    let rec length acc = function
      | Path.Pident _ -> acc + 1
      | Pdot (p, _) | Papply (p, _) | Pextra_ty (p, _) -> length (acc + 1) p

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
