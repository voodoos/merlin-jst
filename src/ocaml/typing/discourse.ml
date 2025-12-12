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

type nonrec t = { paths : t; substs : Lid_set.t Path.Map.t }
let empty = { paths = empty; substs = Path.Map.empty }
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
      (Format.pp_print_list ~pp_sep Pprintast.longident)
      s
  in
  let substs =
    Path.Map.bindings substs |> List.map (fun (p, s) -> (p, Lid_set.elements s))
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

(* TODO: [Discourse.of_core_type] could be entangled in the typer directly. This
   will make these changes more invasive and difficult to reason about but we
   could do it if the current implementation has issues. Notably, all the
   environment lookups are already done at some point in the typer.

   We probably want to do change the typemod and typetexp implementation for the
   final implementation. Careful environement manupilation would  be done only
   once and the visibility of discourses construction might encourage
   maintainers to take it into account when making changes. *)

(* [Discourse.of_core_type] lookup paths appearing in core_types. This is meant
   to gather user-written paths associated to a value or type declaration. These
   paths should be added to the domain of discourse when this value / type is
   used. *)
let of_core_type env ?(acc = Discourse_types.empty) ty =
  let rec aux env acc (ct : Parsetree.core_type) =
    match ct.ptyp_desc with
    | Ptyp_any _ -> acc
    | Ptyp_arrow (_, ct1, ct2, _, _) ->
      let acc = aux env acc ct1 in
      aux env acc ct2
    | Ptyp_tuple l | Ptyp_unboxed_tuple l ->
      List.fold_left (fun acc (_, ct) -> aux env acc ct) acc l
    | Ptyp_constr ({ txt = lid }, params) ->
      let path, _td = Env.find_type_by_name lid env in
      let acc = Discourse_types.Lid_trie.add lid (Type, path) acc in
      List.fold_left (aux env) acc params
    | Ptyp_object (fields, _) ->
      List.fold_left
        (fun acc { Parsetree.pof_desc = Otag (_, ct) | Oinherit ct; _ } ->
          aux env acc ct)
        acc fields
    | Ptyp_class ({ txt = lid; _ }, l) ->
      let path, _td = Env.find_type_by_name lid env in
      let acc = Discourse_types.Lid_trie.add lid (Type, path) acc in
      List.fold_left (aux env) acc l
    | Ptyp_alias (ct, _, _) -> aux env acc ct
    | Ptyp_variant (row, _, _) ->
      List.fold_left
        (fun acc { Parsetree.prf_desc; _ } ->
          match prf_desc with
          | Rtag (_, _, l) -> List.fold_left (aux env) acc l
          | Rinherit ct -> aux env acc ct)
        acc row
    | Ptyp_poly (_, ct) -> aux env acc ct
    | Ptyp_package ({ txt = lid; _ }, l) ->
      let path, _td = Env.find_modtype_by_name lid env in
      let acc = Discourse_types.Lid_trie.add lid (Module_type, path) acc in
      List.fold_left (fun acc (_, ct) -> aux env acc ct) acc l
    | Ptyp_open (lid, ct) ->
      let path, _, newenv =
        Env.open_signature ~used_slot:(ref false) ~toplevel:false ~loc:lid.loc
          Asttypes.Fresh lid env
      in
      let acc = Discourse_types.Lid_trie.add lid.txt (Module, path) acc in
      aux newenv acc ct
    | Ptyp_of_kind _ | Ptyp_var _ -> acc
    | Ptyp_extension _ | Ptyp_quote _ | Ptyp_splice _ -> acc
  in
  (* TODO do we want finer recovery here ? *)
  try aux env acc ty with Not_found | Env.Error (Lookup_error _) -> acc

(** [add_path_to_discourse] adds one path from U to the Discourse, eventually
    adding the additionnal paths described by the rules for D. TODO this could
    and probably should be done lazily.

    TODO:Q what about rule D11 ? If a path is in D and it includes another module
          path within it, then that module path is also in D. Should we consider
          only [Papply] paths or all path components for addition to D ?  *)
let rec add_path_to_discourse ?(for_open = false) env discourse kind lid path =
  log ~title:"add_path_to_discourse" "Adding %s %a %a"
    (Shape.Sig_component_kind.to_string kind)
    Logger.fmt
    (fun fmt -> Pprintast.longident fmt lid)
    Logger.fmt
    (fun fmt -> Path.print fmt path);
  let paths = Lid_trie.add lid (kind, path) discourse.paths in
  let ldot id = if for_open then lid else Longident.Ldot (lid, Ident.name id) in
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
      let paths = Lid_trie.union paths md.md_discourse in
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
                | None -> Some (Lid_set.singleton lid)
                | Some lids -> Some (Lid_set.add lid lids))
              substs
          in
          (paths, substs)
        | Mty_signature s ->
          (* D3. If a module path is in U then all the paths of its subcomponents
             are in D *)
          List.fold_left
            (fun (p, s) ->
              let add kind id =
                log ~title:"add_path_to_discourse"
                  "Adding signature component %s %a"
                  (Shape.Sig_component_kind.to_string kind) Logger.fmt
                  (fun fmt -> Ident.print fmt id);
                Lid_trie.add (ldot id) (kind, pdot id) p
              in
              function
              | Subst.Lazy.Sig_value (id, _, _) -> (add Value id, s)
              | Subst.Lazy.Sig_type (id, _, _, _) -> (add Type id, s)
              | Subst.Lazy.Sig_typext (id, _, _, _) ->
                (add Extension_constructor id, s)
              | Subst.Lazy.Sig_module (id, _, _, _, _) ->
                let d =
                  (* TODO with the loop in add_used this means everything gets
                     added twice... *)
                  add_path_to_discourse env { paths = p; substs = s } Module
                    (ldot id) (pdot id)
                in
                (d.paths, d.substs)
              | Subst.Lazy.Sig_modtype (id, _, _) -> (add Module_type id, s)
              | Subst.Lazy.Sig_class (id, _, _, _) -> (add Class id, s)
              | Subst.Lazy.Sig_class_type (id, _, _, _) -> (add Class_type id, s))
            (paths, substs)
            (Subst.Lazy.force_signature_once s)
        | _ -> (paths, substs)
      end
    | Module_type ->
      let mtd = Env.find_modtype path env in
      (* D8. If a module type path is in U then any paths used in its definition
         are in *)
      (Lid_trie.union paths mtd.mtd_discourse, substs)
    | Value ->
      (* D4. If a value path is in U and its value description was written by a user -
         as opposed to being inferred - then the paths used in that description are
         in D. *)
      let vd = Env.find_value path env in
      (Lid_trie.union paths vd.val_discourse, substs)
    | Type ->
      (* D6. If a type path is in U then any paths used in its equation or
         representation are in D. *)
      let td = Env.find_type path env in
      (Lid_trie.union paths td.type_discourse, substs)
    | _ -> (paths, substs)
  in
  { paths; substs }

(** [add_used] adds all parts of a used path to the Discourse (U1, D2) *)
let add_used env kind lid path =
  let mkloc l = Location.mkloc l lid.Location.loc in
  let rec loop acc kind lid path =
    let lid, loc = (lid.Location.txt, lid.Location.loc) in
    let () = log_usage ~loc kind path in
    let acc =
      try add_path_to_discourse env acc kind lid path with Not_found -> acc
    in
    match ((path : Path.t), (lid : Longident.t)) with
    | Pdot (path, _), Ldot (lid, _) -> loop acc Module (mkloc lid) path
    | Papply (p1, p2), Lapply (l1, l2) ->
      let acc = loop acc Module (mkloc l1) p1 in
      loop acc Module (mkloc l2) p2
    | _, _ -> acc
  in
  if record_usages then g := loop !g kind lid path

(* Rule U2: All paths for definitions in the current file are in U *)
let define kind env_lookup env lid =
  if record_usages then begin
    try
      let path, _ = env_lookup lid env in
      log ~title:"def" "Define %s %a\n%!"
        (Shape.Sig_component_kind.to_string kind) Logger.fmt (fun fmt ->
          Path.print fmt path);
      g := add_path_to_discourse env !g kind lid path
    with Not_found -> ()
  end

let define_type = define Type Env.find_type_by_name
let define_value = define Value Env.find_value_by_name
let define_module = define Module Env.find_module_by_name
let define_modtype = define Module_type Env.find_modtype_by_name

(* Rule U3: All paths for things “defined” using include or open in the current
   file are in U. *)

let define_signature env sg =
  if record_usages then
    let lident id = Longident.Lident (Ident.name id) in
    List.iter
      (function
        | Types.Sig_type (id, _, _, _) -> define_type env (lident id)
        | Types.Sig_value (id, _, _) -> define_value env (lident id)
        | Types.Sig_typext (_, _, _, _) -> ()
        | Types.Sig_module (id, _, _, _, _) -> define_module env (lident id)
        | Types.Sig_modtype (id, _, _) -> define_module env (lident id)
        | Types.Sig_class (_, _, _, _) | Types.Sig_class_type (_, _, _, _) ->
          (* TODO *) ())
      sg

let open_module ~env ~newenv path =
  if record_usages then
    try
      (* When opening we need to traverse the aliases to get the components *)
      let path = Env.normalize_module_path None env path in
      let md = Env.find_module path env in
      match md.md_type with
      | Mty_signature sg -> define_signature newenv sg
      | _ -> ()
    with Not_found -> ()

(* Rule U1: Any path occurring in the file is in U *)
let use_module env lid path = add_used env Module lid path
let use_modtype env lid path = add_used env Module_type lid path
let use_type env lid path = add_used env Type lid path
let use_value env lid path = add_used env Value lid path

let use_constructor _env (constr : Types.constructor_description) =
  if record_usages then begin
    (* If a constructor is in U then any paths used in its type are in D. *)
    g := { !g with paths = Lid_trie.union !g.paths constr.cstr_discourse }
  end

let use_label _env (label : _ Types.gen_label_description) =
  if record_usages then begin
    (* If a label is in U then any paths used in its type are in D. *)
    g := { !g with paths = Lid_trie.union !g.paths label.lbl_discourse }
  end
