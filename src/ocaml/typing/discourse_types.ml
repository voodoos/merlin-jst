(* This module exists to prevent a dependency cycle with Types. *)

let compare_paths ?(compare_idents = Ident.compare)
    ?(compare_strings = String.compare) (p1 : Path.t) (p2 : Path.t) =
  let rec compare_paths (p1 : Path.t) (p2 : Path.t) =
    if p1 == p2 then 0
    else
      match (p1, p2) with
      | Path.Pident id1, Pident id2 -> compare_idents id1 id2
      | Pdot (p1, s1), Pdot (p2, s2) ->
        let h = compare_paths p1 p2 in
        if h <> 0 then h else compare_strings s1 s2
      | Papply (fun1, arg1), Papply (fun2, arg2) ->
        let h = compare_paths fun1 fun2 in
        if h <> 0 then h else compare_paths arg1 arg2
      | Pextra_ty (p1, t1), Pextra_ty (p2, t2) ->
        let h = compare_extra t1 t2 in
        if h <> 0 then h else compare_paths p1 p2
      | Pident _, (Pdot _ | Papply _ | Pextra_ty _)
      | Pdot _, (Papply _ | Pextra_ty _)
      | Papply _, Pextra_ty _ -> -1
      | (Pextra_ty _ | Papply _ | Pdot _), Pident _
      | (Pextra_ty _ | Papply _), Pdot _
      | Pextra_ty _, Papply _ -> 1
  and compare_extra (t1 : Path.extra_ty) (t2 : Path.extra_ty) =
    match (t1, t2) with
    | Pcstr_ty s1, Pcstr_ty s2 -> compare_strings s1 s2
    | Pext_ty, Pext_ty -> 0
    | Punboxed_ty, Punboxed_ty -> 0
    | Pcstr_ty _, (Pext_ty | Punboxed_ty) -> -1
    | (Pext_ty | Punboxed_ty), Pcstr_ty _ -> 1
    | Pext_ty, Punboxed_ty -> -1
    | Punboxed_ty, Pext_ty -> 1
  in
  compare_paths p1 p2

let rec compare_longidents ?(compare_strings = String.compare)
    (l1 : Longident.t) (l2 : Longident.t) =
  match (l1, l2) with
  | Lident s1, Lident s2 -> compare_strings s1 s2
  | Lident _, _ -> -1
  | _, Lident _ -> 1
  | Ldot (l1, s1), Ldot (l2, s2) ->
    let c = compare_longidents l1 l2 in
    if c = 0 then compare_strings s1 s2 else c
  | Lapply (l1, l'1), Lapply (l2, l'2) ->
    let c = compare_longidents l1 l2 in
    if c = 0 then compare_longidents l'1 l'2 else c
  | Ldot _, Lapply _ -> -1
  | Lapply _, Ldot _ -> 1

module Paths = struct
  module T = struct
    type t = Shape.Sig_component_kind.t * Longident.t * Path.t

    (* Since we are versing these paths in a different structure (the
        priority queue) before shortening, it does not seems useful tu use a
        custom path comparison function here. *)

    let compare (_, lid1, p1) (_, lid2, p2) =
      let c = compare_longidents lid1 lid2 in
      if c = 0 then compare_paths p1 p2 else c
  end

  include Set.Make (T)
end

type t = Paths.t
let empty = Paths.empty

let pp ppf t =
  let pp_sep ppf () = Format.fprintf ppf ";@;" in
  let pp_lid_and_path ppf (lid, p) =
    Format.fprintf ppf "%a (%a)" Pprintast.longident lid Path.print p
  in
  let paths = Paths.elements t |> List.map (fun (_, lid, p) -> (lid, p)) in
  Format.pp_print_list ~pp_sep pp_lid_and_path ppf paths

module String_map = Map.Make (String)

module Lid_trie = struct
  type t = Trie of Path.Set.t * t String_map.t

  let rec pp_trie fmt (Trie (paths, tries)) =
    let open Format in
    pp_print_seq Path.print fmt (Path.Set.to_seq paths);
    let pp_map fmt (id, trie) =
      Format.fprintf fmt "@[<v 2>%s: %a@]" id pp_trie trie
    in
    pp_print_seq pp_map fmt (String_map.to_seq tries)

  (* let empty = Trie (Path.Set.empty, String_map.empty) *)

  let leaf path = Trie (Path.Set.singleton path, String_map.empty)

  let trie_of_lid lid path =
    let rec aux acc = function
      | Longident.Lident id ->
        let map = String_map.singleton id acc in
        Trie (Path.Set.empty, map)
      | Ldot (lid, id) ->
        let acc = Trie (Path.Set.empty, String_map.singleton id acc) in
        aux acc lid
      | Lapply (_, _) -> assert false
    in
    aux (leaf path) lid

  let rec union (Trie (p1, m1)) (Trie (p2, m2)) =
    Trie
      ( Path.Set.union p1 p2,
        String_map.union (fun _key t1 t2 -> Some (union t1 t2)) m1 m2 )

  let _ =
    let t =
      union
        (trie_of_lid
           (Ldot (Lident "A", "x"))
           (Pident (Ident.create_persistent "totoid")))
        (trie_of_lid
           (Ldot (Ldot (Lident "A", "B"), "x"))
           (Pident (Ident.create_persistent "totoid2")))
    in
    Format.eprintf "TRIE %a\n%!" pp_trie t
end
