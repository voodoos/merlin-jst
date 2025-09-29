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
let log_section = "discourse"
let { Logger.log } = Logger.for_section log_section

let debug_print fmt = pp fmt !g

let log_usage ?loc kind path =
  log ~title:"use" "Use %a\n%!" Logger.fmt (fun fmt ->
      Format.fprintf fmt "%s %a %a"
        (Shape.Sig_component_kind.to_string kind)
        Path.print path
        (fun fmt -> Format.pp_print_option Location.print_loc fmt)
        loc)

let aggregate_discourse_of_signature sign =
  let open Discourse_types.Paths in
  List.fold_left
    (fun acc -> function
      | Types.Sig_value (_id, vd, _) -> union acc vd.val_discourse
      | Sig_type (_, td, _, _) -> union acc td.type_discourse
      | _ -> acc)
    empty sign

let of_module_type (mt : Types.module_type) =
  match mt with
  | Mty_signature s -> aggregate_discourse_of_signature s
  | _ -> Paths.empty

let () = Subst.discourse_of_module_type := of_module_type

(* [Discourse.of_core_type] lookup paths appearing in core_types. This is meant
   to gather user-written paths associated to a value or type declaration. These
   paths should be added to the domain of discourse when this value / type is
   used. *)
let of_core_type env ?(acc = Discourse_types.empty) ty =
  let rec aux acc (ct : Parsetree.core_type) =
    match ct.ptyp_desc with
    | Ptyp_any _ -> acc
    | Ptyp_arrow (_, ct1, ct2, _, _) ->
      let acc = aux acc ct1 in
      aux acc ct2
    | Ptyp_tuple l | Ptyp_unboxed_tuple l ->
      List.fold_left (fun acc (_, ct) -> aux acc ct) acc l
    | Ptyp_constr ({ txt = lid }, params) ->
      let path, _td = Env.find_type_by_name lid env in
      let acc = Discourse_types.Paths.add (Type, path) acc in
      List.fold_left aux acc params
    | Ptyp_object _ -> (* TODO *) acc
    | Ptyp_class _ -> (* TODO *) acc
    | Ptyp_alias (ct, _, _) -> aux acc ct
    | Ptyp_variant (_row, _, _) -> (* TODO *) acc
    | Ptyp_poly (_, ct) -> aux acc ct
    | Ptyp_package _ -> (* TODO *) acc
    | Ptyp_open (_lid, ct) -> (* TODO *) aux acc ct
    | Ptyp_of_kind _ | Ptyp_var _ -> (* TODO ? *) acc
    | Ptyp_extension _ -> acc
  in
  aux acc ty

(** [add_path_to_discourse] adds one path from U to the Discourse, eventually adding the
    additionnal paths described by the rules for D. TODO this could and probably
    should be done lazily.

    TODO:Q what about rule D11 ? If a path is in D and it includes another module
          path within it, then that module path is also in D. Should we consider
          only [Papply] paths or all path components for addition to D ?  *)
let add_path_to_discourse env paths kind path =
  let paths = Paths.add (kind, path) paths in
  match kind with
  | Module ->
    (* TODO This should probably be done lazily *)
    let md = Env.find_module path env in
    (* D5. If a module path is in U and its module description was written then
       the paths used in that description are in D *)
    let paths = Paths.union paths md.md_discourse in
    begin
      (* D3. If a module path is in U then all the paths of its subcomponents
         are in D *)
      (* TODO Should we tap into Env.module_data instead ? Or should this be
         part of the module discourse in md_discourse ? *)
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
  | Module_type ->
    let mtd = Env.find_modtype path env in
    (* D8. If a module type path is in U then any paths used in its definition
       are in *)
    Paths.union paths mtd.mtd_discourse
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
let add_used ?loc env kind path =
  let rec loop acc kind path =
    let () = log_usage ?loc kind path in
    let acc = add_path_to_discourse env acc kind path in
    match path with
    | Path.Pident _ | Pextra_ty _ -> acc
    | Pdot (path, _) -> loop acc Module path
    | Papply (p1, p2) ->
      let acc = loop acc Module p1 in
      loop acc Module p2
  in
  g := loop !g kind path

(* Rule U2: All paths for definitions in the current file are in U *)
let define_type path =
  log ~title:"def" "Define type %a\n%!" Logger.fmt (fun fmt ->
      Path.print fmt path);
  g := Paths.add (Type, path) !g

let define_module path =
  log ~title:"def" "Define module %a\n%!" Logger.fmt (fun fmt ->
      Path.print fmt path);
  g := Paths.add (Module, path) !g

let define_modtype path =
  log ~title:"def" "Define modtype %a\n%!" Logger.fmt (fun fmt ->
      Path.print fmt path);
  g := Paths.add (Module_type, path) !g

(* Rule U1: Any path occurring in the file is in U *)
let use_module ~loc env path = add_used ~loc env Module path
let use_modtype ~loc env path = add_used ~loc env Module_type path
let use_type ~loc env path = add_used ~loc env Type path

let use_constructor _env (constr : Types.constructor_description) =
  (* If a constructor is in U then any paths used in its type are in D. *)
  g := Paths.union !g constr.cstr_discourse

let use_label _env (label : _ Types.gen_label_description) =
  (* If a label is in U then any paths used in its type are in D. *)
  g := Paths.union !g label.lbl_discourse

let canonical_paths : Paths.t Path.Map.t ref = Local_store.s_ref Path.Map.empty

let graph = Short_paths_graph.Graph.empty
