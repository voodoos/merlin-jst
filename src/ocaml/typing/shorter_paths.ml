let log_section = "short-paths"
let { Logger.log } = Logger.for_section log_section

module Data = struct
  module T = struct
    type t = Path.t

    (* Here we want a compare function that is really based on path lengths.
       TODO the shortest path is not always the one expected by the user. *)

    let compare_idents i1 i2 =
      (* Ths is not a total order on idents right ? *)
      let compare_names = String.compare (Ident.name i1) (Ident.name i2) in
      if compare_names = 0 then Ident.compare i1 i2 else compare_names

    let compare p1 p2 = Discourse_types.compare_paths ~compare_idents p1 p2
  end

  module Map = Map.Make (T)
  module Set = Set.Make (T)
end

module Priority_queue = Data.Set

module Out_type = struct
  (* This part is copied from upstream's [Out_type] *)
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

let rec lid_of_path = function
  | Path.Pident id -> Longident.Lident (Ident.name id)
  | Pdot (p, name) -> Ldot (lid_of_path p, name)
  | Papply (p, p') -> Lapply (lid_of_path p, lid_of_path p')
  | Pextra_ty _ -> assert false

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

let priority_queue : Data.Set.t ref = Local_store.s_ref Priority_queue.empty
let canon_table : Data.Set.t Data.Map.t ref = Local_store.s_ref Data.Map.empty

let normalize_type_path = Out_type.normalize_type_path ~cache:false

let fill_with_discourse (discourse : Discourse.t) queue =
  Discourse_types.Paths.fold
    (fun (kind, path) acc ->
      if kind = Type then Priority_queue.add path acc else acc)
    discourse.paths queue

let apply_substitutions substs queue =
  (* We compute the transitive closure of the queue with regard to path
     substitutions. *)
  (* TODO this is a bit brutal and innefficient. We should think of a better
     algorithm for the final version. *)
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
  let apply_substs acc path substs =
    Path.Map.fold
      (fun target replacements acc ->
        let results = replace_first target ~in_:path ~by:replacements in
        Path.Set.fold Priority_queue.add results acc)
      substs acc
  in
  (* TODO we could only loop on the new productions, not the entire queue *)
  let rec fix queue =
    let new_queue =
      Priority_queue.fold
        (fun path acc -> apply_substs acc path substs)
        queue queue
    in
    if Priority_queue.(cardinal new_queue > cardinal queue) then fix new_queue
    else new_queue
  in
  fix queue

let compare_length p1 p2 =
  let compare_strings s1 s2 = String.length s1 - String.length s2 in
  Discourse_types.compare_paths
    ~compare_idents:(fun id1 id2 ->
      compare_strings (Ident.name id1) (Ident.name id2))
    ~compare_strings p1 p2

let find_best_path env ~canon_path table =
  let is_valid path =
    (* This prevents "shadowing" issues by finding "by name" in the env *)
    match Env.find_type_by_name (lid_of_path path) env with
    | exception Not_found -> false
    | path', _ ->
      let canon', _ = normalize_type_path env path' in
      if Path.compare canon_path canon' == 0 then true else false
  in
  match Data.Map.find_opt canon_path table with
  | None -> None
  | Some paths -> Data.Set.find_first_opt is_valid paths

let process_queue env queue table ~canon_path best_path =
  let rec fill_by_level ~compare seq queue table best_path =
    match (seq (), best_path) with
    | Seq.Nil, _ ->
      log ~title:"fill_one_level" "Empty queue";
      (best_path, queue, table)
    | Seq.Cons (path, _next), Some p when compare path < 0 && compare p >= 0 ->
      log ~title:"fill_one_level"
        "Finished level and found a path shorter than the previous level.";
      (Some p, queue, table)
    | Seq.Cons (path, next), _ ->
      log ~title:"fill_one_level" "Treating %a\n%!" Logger.fmt (fun fmt ->
          Path.print fmt path);
      let canon, _ =
        (* TODO this probably can raise *)
        normalize_type_path env path
      in
      let table =
        (* We add the path to the table for future lookups *)
        Data.Map.update canon
          (function
            | None -> Some (Data.Set.singleton path)
            | Some set -> Some (Data.Set.add path set))
          table
      in
      (* And remove it from the queue *)
      let queue = Priority_queue.remove path queue in
      let best_path =
        (* TODO maybe should check only when changing level ? *)
        if Path.compare canon_path canon <> 0 then best_path
        else begin
          (* Attempt at fixing "shadowing" issues by finding "by name" in the env *)
          match Env.find_type_by_name (lid_of_path path) env with
          | exception Not_found -> best_path
          | path', _ ->
            let canon', _ =
              Out_type.normalize_type_path ~cache:false env path'
            in
            if Path.compare canon canon' == 0 then Some path else best_path
        end
      in
      fill_by_level ~compare:(compare_length path) next queue table best_path
  in
  match (Priority_queue.min_elt_opt queue, best_path) with
  | None, _ -> (best_path, queue, table)
  | Some min_elt, Some best when compare_length min_elt best > 0 ->
    (* There cannot be a better candidate in the queue *)
    (best_path, queue, table)
  | Some min_elt, best_path ->
    let compare = compare_length min_elt in
    let seq = Priority_queue.to_seq queue in
    fill_by_level ~compare seq queue table best_path

let shorten ~env ~canon_path =
  let discourse = Discourse.get () in
  let queue, table = (!priority_queue, !canon_table) in
  let queue =
    queue
    |> fill_with_discourse discourse
    |> apply_substitutions discourse.substs
  in
  (* Do we already have a good candidate ? *)
  let best_path = find_best_path env ~canon_path table in
  (* Is there a better one in the queue ? *)
  let best_path, new_queue, new_table =
    process_queue env queue table ~canon_path best_path
  in
  (* Empty the discourse *)
  Discourse.set { discourse with paths = Discourse_types.Paths.empty };
  (* Update the persistent queue and table *)
  priority_queue := new_queue;
  canon_table := new_table;
  match best_path with
  | None -> canon_path
  | Some path -> path

let find_type_simple ~env path =
  log ~title:"find_type_simple" "Initial: %a\n%!" Logger.fmt (fun fmt ->
      Path.print fmt path);
  let canon_path, _subst = normalize_type_path env path in
  log ~title:"find_type_simple" "Canon: %a\n%!" Logger.fmt (fun fmt ->
      Path.print fmt canon_path);
  let short = shorten ~env ~canon_path in
  log ~title:"find_type_simple" "Short: %a\n%!" Logger.fmt (fun fmt ->
      Path.print fmt short);
  short
