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

open Discourse_types

type t

open Shape.Sig_component_kind
let empty = []

let g = Local_store.s_ref Paths.empty

(** [add_used_path] adds one path from U to the Discourse *)
let add_used_path env paths kind path =
  let paths = Paths.add (kind, path) paths in
  match kind with
  | Module ->
    (* D3. If a module path is in U then all the paths of its subcomponents
       are in D *)
    (* TODO This should probably be done lazily *)
    let md = Env.find_module path env in
    (* TODO Should we tap into Env.module_data instead ? *)
    begin
      match md.md_type with
      | Mty_signature s ->
        List.fold_left
          (fun p -> function
            | Types.Sig_value (id, _, _) ->
              Paths.add (Value, Pdot (path, Ident.name id)) p
            | _ (* TODO *) -> p)
          paths s
      | _ -> paths
    end
  | Value ->
    (* D4. If a value path is in U and its value description was written by a user -
       as opposed to being inferred - then the paths used in that description are
       in D. *)
    let vd = Env.find_value path env in
    Paths.union paths vd.val_discourse
  | Type ->
    (* D6. If a type path is in U then any paths used in its equation or
       representation are in D. *)
    let td = Env.find_type path env in
    Paths.union paths td.type_discourse
  | _ -> paths

(** [add_used] adds all parts of a used path to the Discourse (U1, D2) *)
let add_used env kind path =
  let rec loop acc kind path =
    let acc = add_used_path env acc kind path in
    match path with
    | Path.Pident _ | Pextra_ty _ -> acc
    | Pdot (path, _) -> loop acc Module path
    | Papply (p1, p2) ->
      let acc = loop acc Module p1 in
      loop acc Module p2
  in
  g := loop !g kind path

let add_type path =
  Format.eprintf "Add type %a\n%!" Path.print path;
  g := Paths.add (Type, path) !g
let add_module path = g := Paths.add (Module, path) !g

let canonical_paths : Paths.t Path.Map.t ref = Local_store.s_ref Path.Map.empty

let graph = Short_paths_graph.Graph.empty
