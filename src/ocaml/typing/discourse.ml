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

open Shape.Sig_component_kind
open Discourse_types

type nonrec t = { paths : t; substs : Path.Set.t Path.Map.t }
let empty = { paths = Paths.empty; substs = Path.Map.empty }
let log_section = "discourse"
let { Logger.log } = Logger.for_section log_section

let g = Local_store.s_ref empty
let get () = !g
let set v = g := v
let reset () = g := empty

let record_usages = Config.merlin

let pp_substs fmt substs =
  let pp_sep ppf () = Format.fprintf ppf ";@;" in
  let pp_v fmt (p, s) =
    Format.fprintf fmt "%a -> [%a]" Path.print p
      (Format.pp_print_list ~pp_sep Path.print)
      s
  in
  let substs =
    Path.Map.bindings substs
    |> List.map (fun (p, s) -> (p, Path.Set.elements s))
  in
  Format.pp_print_list ~pp_sep pp_v fmt substs

let debug_print fmt =
  Format.fprintf fmt "%a@;%a" pp !g.paths pp_substs !g.substs

let log_usage ?loc kind path =
  log ~title:"use" "Use %a\n%!" Logger.fmt (fun fmt ->
      Format.fprintf fmt "%s %a %a"
        (Shape.Sig_component_kind.to_string kind)
        Path.print path
        (fun fmt -> Format.pp_print_option Location.print_loc fmt)
        loc)

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
      let acc = Discourse_types.Paths.add (Type, lid, path) acc in
      List.fold_left aux acc params
    | Ptyp_object (fields, _) ->
      List.fold_left
        (fun acc { Parsetree.pof_desc = Otag (_, ct) | Oinherit ct; _ } ->
          aux acc ct)
        acc fields
    | Ptyp_class ({ txt = lid; _ }, l) ->
      let path, _td = Env.find_type_by_name lid env in
      let acc = Discourse_types.Paths.add (Type, lid, path) acc in
      List.fold_left aux acc l
    | Ptyp_alias (ct, _, _) -> aux acc ct
    | Ptyp_variant (row, _, _) ->
      List.fold_left
        (fun acc { Parsetree.prf_desc; _ } ->
          match prf_desc with
          | Rtag (_, _, l) -> List.fold_left aux acc l
          | Rinherit ct -> aux acc ct)
        acc row
    | Ptyp_poly (_, ct) -> aux acc ct
    | Ptyp_package ({ txt = lid; _ }, l) ->
      let path, _td = Env.find_modtype_by_name lid env in
      let acc = Discourse_types.Paths.add (Module_type, lid, path) acc in
      List.fold_left (fun acc (_, ct) -> aux acc ct) acc l
    | Ptyp_open ({ txt = lid; _ }, ct) ->
      let path, _td = Env.find_module_by_name lid env in
      let acc = Discourse_types.Paths.add (Module, lid, path) acc in
      aux acc ct
    | Ptyp_of_kind _ | Ptyp_var _ -> acc
    | Ptyp_extension _ -> acc
  in
  aux acc ty

(** [add_path_to_discourse] adds one path from U to the Discourse, eventually adding the
    additionnal paths described by the rules for D. TODO this could and probably
    should be done lazily.

    TODO:Q what about rule D11 ? If a path is in D and it includes another module
          path within it, then that module path is also in D. Should we consider
          only [Papply] paths or all path components for addition to D ?  *)
let rec add_path_to_discourse env discourse kind lid path =
  let paths = Paths.add (kind, lid, path) discourse.paths in
  let ldot id = Longident.Ldot (lid, Ident.name id) in
  let pdot id = Path.Pdot (path, Ident.name id) in
  let paths, substs =
    let substs = discourse.substs in
    match kind with
    | Module ->
      (* TODO This should probably be done lazily *)
      let md = Env.find_module_lazy path env in
      (* D5. If a module path is in U and its module description was written then
         the paths used in that description are in D *)
      (* TODO this should call add_path_to_discourse *)
      let paths = Paths.union paths md.md_discourse in
      begin
        match md.md_type with
        | Mty_alias p ->
          (* D12. If a module path m in D - note D not U - is a module alias
             with target n and another path p in D includes n within it, then
             the path obtained by substituting the m for n in p is also in D.

             We accumulate such substitution and will apply them when shortening
             a path. *)
          let substs =
            Path.Map.update p
              (function
                | None -> Some (Path.Set.singleton path)
                | Some paths -> Some (Path.Set.add path paths))
              substs
          in
          (paths, substs)
        | Mty_signature s ->
          (* D3. If a module path is in U then all the paths of its subcomponents
             are in D *)
          List.fold_left
            (fun (p, s) -> function
              | Subst.Lazy.Sig_value (id, _, _) ->
                (Paths.add (Value, ldot id, pdot id) p, s)
              | Subst.Lazy.Sig_type (id, _, _, _) ->
                (Paths.add (Type, ldot id, pdot id) p, s)
              | Subst.Lazy.Sig_typext (id, _, _, _) ->
                (Paths.add (Extension_constructor, ldot id, pdot id) p, s)
              | Subst.Lazy.Sig_module (id, _, _, _, _) ->
                let d =
                  add_path_to_discourse env { paths = p; substs = s } Module
                    (ldot id) (pdot id)
                in
                (d.paths, d.substs)
              | Subst.Lazy.Sig_modtype (id, _, _) ->
                (Paths.add (Module_type, ldot id, pdot id) p, s)
              | Subst.Lazy.Sig_class (id, _, _, _) ->
                (Paths.add (Class, ldot id, pdot id) p, s)
              | Subst.Lazy.Sig_class_type (id, _, _, _) ->
                (Paths.add (Class_type, ldot id, pdot id) p, s))
            (paths, substs)
            (Subst.Lazy.force_signature_once s)
        | _ -> (paths, substs)
      end
    | Module_type ->
      let mtd = Env.find_modtype path env in
      (* D8. If a module type path is in U then any paths used in its definition
         are in *)
      (Paths.union paths mtd.mtd_discourse, substs)
    | Value ->
      (* D4. If a value path is in U and its value description was written by a user -
         as opposed to being inferred - then the paths used in that description are
         in D. *)
      let vd = Env.find_value path env in
      (Paths.union paths vd.val_discourse, substs)
    | Type ->
      (* D6. If a type path is in U then any paths used in its equation or
         representation are in D. *)
      let td = Env.find_type path env in
      (Paths.union paths td.type_discourse, substs)
    | _ -> (paths, substs)
  in
  { paths; substs }

(** [add_used] adds all parts of a used path to the Discourse (U1, D2) *)
let add_used env kind lid path =
  let mkloc l = Location.mkloc l lid.Location.loc in
  let rec loop acc kind lid path =
    let () = log_usage ~loc:lid.Location.loc kind path in
    let acc = add_path_to_discourse env acc kind lid.txt path in
    match ((path : Path.t), (lid.txt : Longident.t)) with
    | Pdot (path, _), Ldot (lid, _) -> loop acc Module (mkloc lid) path
    | Papply (p1, p2), Lapply (l1, l2) ->
      let acc = loop acc Module (mkloc l1) p1 in
      loop acc Module (mkloc l2) p2
    | _, _ -> acc
  in
  if record_usages then g := loop !g kind lid path

(* Rule U2: All paths for definitions in the current file are in U *)
let define_type env lid =
  if record_usages then begin
    let path, _ = Env.find_type_by_name lid env in
    log ~title:"def" "Define type %a\n%!" Logger.fmt (fun fmt ->
        Path.print fmt path);
    g := { !g with paths = Paths.add (Type, lid, path) !g.paths }
  end

let define_module env lid =
  if record_usages then begin
    let path, _ = Env.find_module_by_name lid env in
    log ~title:"def" "Define module %a\n%!" Logger.fmt (fun fmt ->
        Path.print fmt path);
    g := add_path_to_discourse env !g Module lid path
  end

let define_modtype env lid =
  if record_usages then begin
    let path, _ = Env.find_modtype_by_name lid env in
    log ~title:"def" "Define modtype %a\n%!" Logger.fmt (fun fmt ->
        Path.print fmt path);
    g := { !g with paths = Paths.add (Module_type, lid, path) !g.paths }
  end

(* Rule U1: Any path occurring in the file is in U *)
let use_module env lid path = add_used env Module lid path
let use_modtype env lid path = add_used env Module_type lid path
let use_type env lid path = add_used env Type lid path
let use_value env lid path = add_used env Value lid path

let use_constructor _env (constr : Types.constructor_description) =
  if record_usages then begin
    (* If a constructor is in U then any paths used in its type are in D. *)
    g := { !g with paths = Paths.union !g.paths constr.cstr_discourse }
  end

let use_label _env (label : _ Types.gen_label_description) =
  if record_usages then begin
    (* If a label is in U then any paths used in its type are in D. *)
    g := { !g with paths = Paths.union !g.paths label.lbl_discourse }
  end
