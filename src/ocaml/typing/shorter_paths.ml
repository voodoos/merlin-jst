let log_section = "short-paths"
let { Logger.log } = Logger.for_section log_section
let { Logger.log = log_dbg } = Logger.for_section (log_section ^ "-dbg")

module Out_type = struct
  (* This part is copied from upstream's [Out_type] *)

  (* Normalize paths *)

  type param_subst = Id | Nth of int | Map of int list

  let compose l1 = function
    | Id -> Map l1
    | Map l2 -> Map (List.map (List.nth l1) l2)
    | Nth n -> Nth (List.nth l1 n)

  let rec index l x =
    match l with
    | [] -> raise Not_found
    | a :: l -> if Types.eq_type x a then 0 else 1 + index l x

  let rec uniq = function
    | [] -> true
    | a :: l -> (not (List.memq (a : int) l)) && uniq l

  let rec normalize_type_path ?(cache = false) env p =
    let open Types in
    try
      let params (* type_params *), ty (* type_manifest *), _ =
        (* Find the manifest type associated to a type when appropriate:
           - the type should be public or should have a private row,
           - the type should have an associated manifest type. *)
        Env.find_type_expansion p env
      in
      match get_desc ty with
      | Tconstr (p1, tyl, _) ->
        log ~title:"normalize_type_path" "Found type expansion %a for %a"
          Logger.fmt (Fun.flip Path.print p1) Logger.fmt (Fun.flip Path.print p);
        if
          List.length params = List.length tyl
          && List.for_all2 eq_type params tyl
          && p1 <> p (* TODO if this the correct way to prevent looping ? *)
        then normalize_type_path ~cache env p1
        else if
          cache
          || List.length params <= List.length tyl
          || not (uniq (List.map get_id tyl))
        then (p, Id)
        else
          let l1 = List.map (index params) tyl in
          let p2, s2 = normalize_type_path ~cache env p1 in
          (p2, compose l1 s2)
      | _ -> (p, Nth (index params ty))
    with Not_found ->
      log ~title:"normalize_type_path"
        "Calling [Env.normalize_type_path] for %a" Logger.fmt
        (Fun.flip Path.print p);
      (Env.normalize_type_path None env p, Id)

  let find_double_underscore s =
    let len = String.length s in
    let rec loop i =
      if i + 1 >= len then None
      else if s.[i] = '_' && s.[i + 1] = '_' then Some i
      else loop (i + 1)
    in
    loop 0

  let penalty s =
    if s <> "" && s.[0] = '_' then 10
    else
      match find_double_underscore s with
      | None -> 1
      | Some _ -> 10
end

let rec longident_cost = function
  | Longident.Lident id -> Out_type.penalty id
  | Ldot (l, _) -> 1 + longident_cost l
  | Lapply (l1, l2) -> longident_cost l1 + longident_cost l2

let compare_strings s1 s2 =
  let length_diff = String.length s1 - String.length s2 in
  if length_diff <> 0 then length_diff else String.compare s1 s2

let compare_longidents l1 l2 =
  let l1_cost = longident_cost l1 in
  let l2_cost = longident_cost l2 in
  let cost_diff = l1_cost - l2_cost in
  if cost_diff <> 0 then cost_diff
  else Discourse_types.compare_longidents ~compare_strings l1 l2

module Lid_path_set = struct
  module T = struct
    type t = Longident.t * Path.t

    let compare (l1, p1) (l2, p2) =
      let c = compare_longidents l1 l2 in
      if c = 0 then Path.compare p1 p2 else c
  end
  let pp_elt fmt (l, p) =
    Format.fprintf fmt "%a (%a)" Pprintast.longident l Path.print p

  include Set.Make (T)
end

module Priority_queue = Lid_path_set

(* To shorten paths we will build a table that maps "canonical paths" - i.e. a
   path which is not itself an alias - to a set of paths from D that are aliases
   to it. We will build this table by putting the elements of D into a priority
   queue ordered by their length. Then we "process" each element by looking up
   its canonical path and then adding it to the map. We do this using whatever
   environment we are currently trying to shorten paths with (ignoring any local
   constraints). If an element of D is not available in that environment then we
   skip it and leave it in the priority queue. *)

(* When shortenning:
   - We empty the Discourse into the priority queue.
   - We compute all possible paths substitutions and add them to the priority
     queue. TODO: there might be a better, lazier way.
   - We read the priority queue in orderto sort paths by their canonical form.
   - Once we found a valid path for the request we stop after finishing the
     current level (when the next path in the queue is longer).
   - The priority queue and canonical paths map are kept between queries. *)

let priority_queue : Priority_queue.t ref =
  Local_store.s_ref Priority_queue.empty
let not_in_env : Priority_queue.t ref = Local_store.s_ref Priority_queue.empty
let canon_table : Lid_path_set.t Path.Map.t ref =
  Local_store.s_ref Path.Map.empty

(* This function should be called before any attempt to [shorten] paths in an
   environment different that the one of the previous attempt.

   This is called after any function wrapped with [Printyp.wrap_printing_env].
*)
let restore_ignored_paths () =
  priority_queue := Priority_queue.union !not_in_env !priority_queue;
  not_in_env := Priority_queue.empty

let pp_table fmt t =
  Path.Map.iter
    (fun p lids ->
      let lids = Lid_path_set.to_list lids in
      Format.fprintf fmt "@;%a -> {%a}" Path.print p
        (Format.pp_print_list ~pp_sep:Format.pp_print_space Lid_path_set.pp_elt)
        lids)
    t

let normalize_type_path = Out_type.normalize_type_path ~cache:false

open Discourse_types

let rec apply_one_substitution ~target ~replacements
    (Trie (paths, children) as t : Discourse_types.t) =
  match Lid_trie.reach t target with
  | Some (Trie (paths, children)) ->
    List.fold_left
      (fun acc lid ->
        Lid_trie.union acc @@ Lid_trie.trie_of_lid ~children lid paths)
      Lid_trie.empty replacements
  | None ->
    let children =
      String_map.filter_map
        (fun _ t ->
          let result = apply_one_substitution ~target ~replacements t in
          if Lid_trie.is_empty result then None else Some result)
        children
    in
    Trie (paths, children)

let apply_substitutions substs t =
  List.fold_left
    (fun acc (target, replacements) ->
      Lid_trie.union acc @@ apply_one_substitution t ~target ~replacements)
    Lid_trie.empty substs

let apply_substitutions_fixpoint t substs =
  let substs =
    Path.Map.bindings substs
    |> List.map (fun (target, replacements) ->
           (Untypeast.lident_of_path target, Lid_set.elements replacements))
  in
  let rec aux acc t =
    let new_lids = apply_substitutions substs t in
    if Lid_trie.is_empty new_lids then acc
    else begin
      let acc_length = Lid_trie.size acc in
      let acc = Lid_trie.union acc new_lids in
      if acc_length = Lid_trie.size acc then acc else aux acc new_lids
    end
  in
  aux t t

let fill_queue (paths : Lid_trie.t) queue =
  Discourse_types.Lid_trie.to_seq paths
  |> Seq.fold_left
       (fun acc (lid, paths) ->
         Discourse_types.Paths.fold
           (fun (kind, path) acc ->
             if kind = Type then Priority_queue.add (lid, path) acc else acc)
           paths acc)
       queue

let find_best_lid env ~canon_path table =
  (* TODO it might be worth it to memoïze this function *)
  let is_valid (lid, path) =
    (* This prevents "shadowing" issues by finding "by name" in the env *)
    match Env.find_type_by_name lid env with
    | exception Not_found -> None
    | path_in_env, _ ->
      let path, _ = normalize_type_path env path in
      let path', _ = normalize_type_path env path_in_env in
      if Path.compare path path' == 0 then Some (lid, path_in_env) else None
  in
  match Path.Map.find_opt canon_path table with
  | None -> None
  | Some lids -> Lid_path_set.to_seq lids |> Seq.find_map is_valid

let improve_lid env ~canon_path table lid =
  find_best_lid env ~canon_path table |> Option.fold ~none:lid ~some:Option.some

type state =
  { queue : Priority_queue.t;
    not_in_env : Priority_queue.t;
    table : Lid_path_set.t Path.Map.t
  }
let process_queue env state ~canon_path best_lid =
  let rec fill_by_level ~compare seq state best_lid =
    log ~title:"fill_by_level" "Current best: %a" Logger.fmt (fun f ->
        Format.pp_print_option Lid_path_set.pp_elt f best_lid);
    match seq () with
    | Seq.Nil ->
      log ~title:"fill_by_level" "Empty queue";
      let best_path = improve_lid env ~canon_path state.table best_lid in
      (best_path, state)
    | Seq.Cons ((lid, path), next) ->
      let next_level = compare lid > 0 in
      if next_level then begin
        match improve_lid env ~canon_path state.table best_lid with
        | Some (lid, path) when compare lid <= 0 ->
          log ~title:"fill_by_level"
            "Finished level and found a name shorter than the previous level:\n\
            \ %a (%a)" Logger.fmt
            (fun fmt -> Pprintast.longident fmt lid)
            Logger.fmt (Fun.flip Path.print path);
          (Some (lid, path), state)
        | best_lid ->
          log ~title:"fill_by_level" "Finished a level. Current best: %a"
            Logger.fmt (fun f ->
              Format.pp_print_option Lid_path_set.pp_elt f best_lid);
          add_lid_to_table state (lid, path) next best_lid
      end
      else add_lid_to_table state (lid, path) next best_lid
  and add_lid_to_table state (lid, path) next best_lid =
    log ~title:"fill_by_level" "Treating %a (%a)" Logger.fmt
      (fun fmt -> Pprintast.longident fmt lid)
      Logger.fmt
      (fun fmt -> Path.print fmt path);
    let state =
      try
        let _check_validity =
          (* In the presence of `open` statements the Discourse contains partial
             paths that cannot be looked-up in the environement directly. We can
             find by name instead. However we must then check that we did actually
             find the type we were looking (same ident) for and not an homonym. *)
          let path', _ = Env.find_type_by_name lid env in
          log ~title:"fill_by_level" "find_type_by_name %a (%a) = %a" Logger.fmt
            (fun fmt -> Pprintast.longident fmt lid)
            Logger.fmt
            (fun fmt -> Path.print fmt path)
            Logger.fmt
            (fun fmt -> Path.print fmt path');

          let p, _ = normalize_type_path env path in
          let p', _ = normalize_type_path env path' in
          log ~title:"fill_by_level" "find_type_by_name %a <>? %a" Logger.fmt
            (fun fmt -> Path.print fmt p)
            Logger.fmt
            (fun fmt -> Path.print fmt p');
          if p' <> p then raise Not_found
        in
        let canon, _ = normalize_type_path env path in
        log ~title:"fill_by_level" "Found canonical path %a" Logger.fmt
          (fun fmt -> Path.print fmt canon);
        let table =
          Path.Map.update canon
            (function
              | None -> Some (Lid_path_set.singleton (lid, path))
              | Some set -> Some (Lid_path_set.add (lid, path) set))
            state.table
        in
        (* And remove it from the queue *)
        let queue = Priority_queue.remove (lid, path) state.queue in
        { state with queue; table }
      with Not_found ->
        log ~title:"fill_by_level" "Name: %a invalid in the current env"
          Logger.fmt (fun fmt -> Pprintast.longident fmt lid);

        (* Elements not valid in the current environement are kept in a separate
           queue. This prevent uselessly re-testing them when [shorten] is
           called multiple times with the same environment. This notably happens
           when printing module signatures, and they can be quite large. *)
        let queue = Priority_queue.remove (lid, path) state.queue in
        let not_in_env = Priority_queue.add (lid, path) state.not_in_env in
        { state with queue; not_in_env }
    in
    fill_by_level ~compare:(compare_longidents lid) next state best_lid
  in
  match (Priority_queue.min_elt_opt state.queue, best_lid) with
  | None, _ -> (best_lid, state)
  | Some (shortest_lid_in_queue, _path), Some (best_lid', _)
    when compare_longidents shortest_lid_in_queue best_lid' > 0 ->
    (* There cannot be a better candidate in the queue *)
    log ~title:"process_queue" "No shorter longidents in the queue. (> %a)"
      Logger.fmt
    @@ Fun.flip Pprintast.longident shortest_lid_in_queue;
    (best_lid, state)
  | Some (shortest_lid, _path), best_lid ->
    let compare = compare_longidents shortest_lid in
    let seq = Priority_queue.to_seq state.queue in
    fill_by_level ~compare seq state best_lid

(* Given a Path and a Longident that represents a suffix  of that path,
   [path_mask] returns the paths corresponding to that suffix. *)

(* This cache prevent the same ident to be given different stamps in the same query *)
(* todo: we should probably store the masking in the table instead. *)
let path_masks_cache : (Longident.t * Path.t, Path.t) Hashtbl.t =
  Hashtbl.create 32

let rec path_mask (path : Path.t) (lid : Longident.t) : Path.t =
  match Hashtbl.find_opt path_masks_cache (lid, path) with
  | Some path -> path
  | None ->
    let masked_path : Path.t =
      match (path, lid) with
      | Pident id, Lident _ -> Pident id
      | Pdot (p, s), Ldot (l, _) -> Pdot (path_mask p l, s)
      | Papply (p1, p2), Lapply (l1, _) -> Papply (path_mask p1 l1, p2)
      | (Pdot _ | Papply _), Lident s ->
        let scope = Path.scope path in
        Pident (Ident.create_scoped ~scope s)
      | _ -> assert false
    in
    Hashtbl.add path_masks_cache (lid, path) masked_path;
    masked_path

let shorten ~env ~canon_path =
  let discourse = Discourse.get () in
  let queue, table = (!priority_queue, !canon_table) in
  log_dbg ~title:"shorten" "Current discourse: %a" Logger.fmt (fun fmt ->
      Discourse.debug_print fmt);
  log_dbg ~title:"shorten" "Current queue: %a" Logger.fmt (fun fmt ->
      Format.pp_print_seq ~pp_sep:Format.pp_print_space Lid_path_set.pp_elt fmt
        (Priority_queue.to_seq queue));
  log_dbg ~title:"shorten" "Current notinenv: %a" Logger.fmt (fun fmt ->
      Format.pp_print_seq ~pp_sep:Format.pp_print_space Lid_path_set.pp_elt fmt
        (Priority_queue.to_seq !not_in_env));
  log_dbg ~title:"shorten" "Current table: %a" Logger.fmt (fun fmt ->
      pp_table fmt table);

  let queue =
    let paths =
      add
        (Untypeast.lident_of_path canon_path)
        (Type, canon_path) discourse.paths
    in
    let paths = apply_substitutions_fixpoint paths discourse.substs in
    fill_queue paths queue
  in

  (* Do we already have a candidate ? *)
  let best_lid = find_best_lid env ~canon_path table in

  log ~title:"shorten" "Initial best: %a" Logger.fmt (fun f ->
      Format.pp_print_option Lid_path_set.pp_elt f best_lid);

  (* Is there a better one in the queue ? *)
  let best_lid, { queue = queue'; not_in_env = not_in_env'; table = table' } =
    let not_in_env = !not_in_env in
    process_queue env { queue; not_in_env; table } ~canon_path best_lid
  in

  (* Empty the discourse *)
  Discourse.set { discourse with paths = Discourse_types.empty };

  (* Update the persistent queues and table *)
  priority_queue := queue';
  not_in_env := not_in_env';
  canon_table := table';

  match best_lid with
  | None -> canon_path
  | Some (lid, path) ->
    log ~title:"shorten" "%a" Logger.fmt (fun fmt ->
        Format.fprintf fmt "Masking path %a with lid %a" Path.print path
          Pprintast.longident lid);
    path_mask path lid

let find_type env path : Short_paths.type_result =
  log ~title:"find_type" "Initial: %a\n%!" Logger.fmt (fun fmt ->
      Path.print fmt path);
  match normalize_type_path env path with
  | _, Nth i -> Nth i
  | canon_path, Id -> Path (None, shorten ~env ~canon_path)
  | canon_path, Map l -> Path (Some l, shorten ~env ~canon_path)

let find_type_resolution env path : Short_paths.type_resolution =
  match normalize_type_path env path with
  | _, Nth i -> Nth i
  | _, Id -> Id
  | _, Map l -> Subst l

let find_type_simple env path =
  log ~title:"find_type_simple" "Initial: %a\n%!" Logger.fmt (fun fmt ->
      Path.print fmt path);
  let canon_path, _subst = normalize_type_path env path in
  log ~title:"find_type_simple" "Canon: %a\n%!" Logger.fmt (fun fmt ->
      Path.print fmt canon_path);
  let short = shorten ~env ~canon_path in
  log ~title:"find_type_simple" "Short: %a\n%!" Logger.fmt (fun fmt ->
      Path.print fmt short);
  short
