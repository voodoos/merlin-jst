let log_section = "short-paths"
let { Logger.log } = Logger.for_section log_section
module Priority_queue = struct
  module T = struct
    type t = Path.t

    let compare = Discourse_types.Paths.T.compare_paths
  end

  include Set.Make (T)
end

module Out_type = struct
  (* coming from upstream Out_type*)
  let find_double_underscore s =
    let len = String.length s in
    let rec loop i =
      if i + 1 >= len then None
      else if s.[i] = '_' && s.[i + 1] = '_' then Some i
      else loop (i + 1)
    in
    loop 0

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
      let params, ty, _ =
        (* Find the manifest type associated to a type when appropriate:
           - the type should be public or should have a private row,
           - the type should have an associated manifest type. *)
        Env.find_type_expansion p env
      in
      match get_desc ty with
      | Tconstr (p1, tyl, _) ->
        if
          List.length params = List.length tyl
          && List.for_all2 eq_type params tyl
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
    with Not_found -> (Env.normalize_type_path None env p, Id)

  let penalty s =
    if s <> "" && s.[0] = '_' then 10
    else
      match find_double_underscore s with
      | None -> 1
      | Some _ -> 10

  let rec path_size = function
    | Path.Pident id -> (penalty (Ident.name id), -Ident.scope id)
    | Pdot (p, _) | Pextra_ty (p, Pcstr_ty _) ->
      let l, b = path_size p in
      (1 + l, b)
    | Papply (p1, p2) ->
      let l, b = path_size p1 in
      (l + fst (path_size p2), b)
    | Pextra_ty (p, _) -> path_size p
end

(* To shorten paths we will build a table that maps "canonical paths" - i.e. a
   path which is not itself an alias - to a set of paths from D that are aliases
   to it. We will build this table by putting the elements of D into a priority
   queue ordered by their length. Then we "process" each element by looking up
   its canonical path and then adding it to the map. We do this using whatever
   environment we are currently trying to shorten paths with (ignoring any local
   constraints). If an element of D is not available in that environment then we
   skip it and leave it in the priority queue. *)
let shorten ~env ~canonical_path =
  let queue =
    Discourse_types.Paths.fold
      (fun (kind, path) acc ->
        if kind = Type then Priority_queue.add path acc else acc)
      !Discourse.g.paths Priority_queue.empty
  in

  let queue =
    (* TODO this is a bit brutal and innefficient. We should think of a better
       algorithm for the real version. *)
    let rec reroot ~root (path : Path.t) =
      match path with
      | Pident _ -> root
      | Pdot (p, n) -> Path.Pdot (reroot ~root p, n)
      | p -> (* Replacement paths cannot be Papply or Pextras *) p
    in
    let replace_first target_path ~in_:path ~by:replacement_paths =
      (* target: M.N  ([Pdot (Pident "M", "N")])
         path L.M.N.v ([Pdot (Pdot (Pdot (Pident "L", "M"), "N"), "v")]) *)
      let rec replace_head (path : Path.t) (remaining_target : Path.t) =
        match (path, remaining_target) with
        | Pident id, Pident id' when Ident.same id id' -> Some replacement_paths
        | Pident _id, _ -> None
        | Pdot (p, n), Pident id ->
          if String.equal n (Ident.name id) then
            Some (Path.Set.map (reroot ~root:p) replacement_paths)
          else None
        | Pdot (p, n), Pdot (t, n') ->
          if String.equal n n' then begin
            replace_head p t
          end
          else None
        | _, (Papply _ | Pextra_ty _) | (Papply _ | Pextra_ty _), _ -> None
      and aux (path : Path.t) =
        match (replace_head path target_path, path) with
        | Some paths, _ -> paths
        | None, Pident _ -> Path.Set.singleton path
        | None, Pdot (p, n) -> Path.Set.map (fun p -> Path.Pdot (p, n)) (aux p)
        | None, Papply (p, p') ->
          let ps = aux p in
          let p's = aux p' in
          Path.Set.fold
            (fun p acc ->
              Path.Set.fold
                (fun p' acc -> Path.Set.add (Papply (p, p')) acc)
                p's acc)
            ps Path.Set.empty
        | None, Pextra_ty _ -> Path.Set.singleton path
      in
      aux path
    in
    let apply_subts acc path substs =
      Path.Map.fold
        (fun target replacements acc ->
          let results = replace_first target ~in_:path ~by:replacements in
          Path.Set.fold Priority_queue.add results acc)
        substs acc
    in
    let rec fix queue =
      let new_queue =
        Priority_queue.fold
          (fun path acc -> apply_subts acc path !Discourse.g.substs)
          queue Priority_queue.empty
      in
      if Priority_queue.(cardinal new_queue > cardinal queue) then fix new_queue
      else new_queue
    in
    fix queue
  in

  (* Todo: actually fill a map one length at a time. This is just for ealry
     testing. *)
  let rec fill_map next =
    match next () with
    | Seq.Nil -> canonical_path
    | Cons (path, next) ->
      log ~title:"fill_map" "Treating %a\n%!" Logger.fmt (fun fmt ->
          Path.print fmt path);
      let canon, _ =
        (* TODO this probably can raise *)
        Out_type.normalize_type_path ~cache:false env path
      in
      if Path.compare canonical_path canon == 0 then path else fill_map next
  in

  fill_map (Priority_queue.to_seq queue)

let find_type_simple ~env path =
  log ~title:"find_type_simple" "Initial: %a\n%!" Logger.fmt (fun fmt ->
      Path.print fmt path);
  let canonical_path, _subst =
    Out_type.normalize_type_path ~cache:false env path
  in
  log ~title:"find_type_simple" "Canon: %a\n%!" Logger.fmt (fun fmt ->
      Path.print fmt canonical_path);
  let short = shorten ~env ~canonical_path in
  log ~title:"find_type_simple" "Short: %a\n%!" Logger.fmt (fun fmt ->
      Path.print fmt short);
  short
