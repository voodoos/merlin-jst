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
    type t = Shape.Sig_component_kind.t * Path.t

    (* Since we are versing these paths in a different structure (the
        priority queue) before shortening, it does not seems useful tu use a
        custom path comparison function here. *)

    let compare (_, p1) (_, p2) = compare_paths p1 p2
  end

  include Set.Make (T)
end

let pp_paths ppf t =
  let pp_sep ppf () = Format.fprintf ppf ";@;" in
  let paths = Paths.elements t |> List.map (fun (_, p) -> p) in
  Format.pp_print_list ~pp_sep Path.print ppf paths

module String_map = Map.Make (String)

module Lid_trie = struct
  type t = Trie of Longident.t option * Paths.t * t String_map.t

  let pp_lid_paths fmt (lid, paths) =
    let open Format in
    fprintf fmt "(%a,[%a])" Pprintast.longident lid pp_paths paths

  let rec pp fmt (Trie (lid, paths, tries)) =
    let open Format in
    let pp_map fmt (id, trie) =
      Format.fprintf fmt "@[<v 2>%s: %a@]" id pp trie
    in
    let pp_lid_paths fmt (lid, paths) =
      match lid with
      | None -> Format.fprintf fmt "Root"
      | Some lid -> pp_lid_paths fmt (lid, paths)
    in
    Format.fprintf fmt "%a :> %a" pp_lid_paths (lid, paths)
      (pp_print_seq pp_map) (String_map.to_seq tries)

  let empty = Trie (None, Paths.empty, String_map.empty)

  let node ?(children = String_map.empty) lid paths =
    Trie (Some lid, paths, children)

  let trie_of_lid ?children lid paths =
    let rec aux acc lid =
      match lid with
      | Longident.Lident id ->
        let map = String_map.singleton id acc in
        Trie (None, Paths.empty, map)
      | Ldot (lid, id) ->
        let acc = Trie (Some lid, Paths.empty, String_map.singleton id acc) in
        aux acc lid
      | Lapply (lid, arg_lid) ->
        let arg =
          let acc =
            Trie (Some arg_lid, Paths.empty, String_map.singleton ")" acc)
          in
          aux acc arg_lid
        in
        aux (Trie (Some lid, Paths.empty, String_map.singleton "(" arg)) lid
    in
    aux (node ?children lid paths) lid

  let rec union (Trie (lid1, p1, m1)) (Trie (lid2, p2, m2)) =
    assert (lid1 = lid2);
    (* TODO remove this assert when it's clear that this invariant holds *)
    Trie
      ( lid1,
        Paths.union p1 p2,
        String_map.union (fun _key t1 t2 -> Some (union t1 t2)) m1 m2 )

  let add lid path t =
    let t' = trie_of_lid lid (Paths.singleton path) in
    union t t'

  let rec reach (Trie (_, _, tries) as t) lid =
    match (lid : Longident.t) with
    | Lident name -> String_map.find_opt name tries
    | Ldot (lid, name) ->
      let parent = reach t lid in
      Option.bind parent (fun (Trie (_, _, tries)) ->
          String_map.find_opt name tries)
    | Lapply (lid, arg_lid) ->
      let parent = reach t lid in
      Option.bind parent (fun (Trie (_, _, tries)) ->
          let arg_trie = String_map.find_opt "(" tries in
          let arg_enc = Option.bind arg_trie (fun t -> reach t arg_lid) in
          Option.bind arg_enc (fun (Trie (_, _, tries)) ->
              String_map.find_opt ")" tries))

  let to_seq t =
    let rec aux lid_acc (Trie (_, _, tries)) =
      String_map.to_seq tries
      |> Seq.flat_map @@ fun (name, t) ->
         let lid =
           (* This is a it convoluted due to the handling of Lapply. *)
           match (lid_acc, name) with
           | None :: tl, _ -> Some (Longident.Lident name) :: tl
           | l, "(" -> None :: l
           | Some arg :: Some lid :: tl, ")" ->
             Some (Longident.Lapply (lid, arg)) :: tl
           | Some lid :: tl, _ -> Some (Longident.Ldot (lid, name)) :: tl
           | _ -> assert false
         in
         Seq.cons (List.hd lid, t) (aux lid t)
    in
    aux [ None ] t
    |> Seq.filter_map (fun (lid, Trie (_lid, paths, _tries)) ->
           if Paths.is_empty paths then None
           else
             (* There must be a lid when paths are stored. *)
             Some (Option.get lid, paths))

  (* let _ =
     let t =
       empty
       |> add
            (Ldot (Lapply (Lident "F", Lident "A"), "x"))
            (Value, Pident (Ident.create_persistent "totoid"))
       |> add
            (Ldot (Ldot (Lident "F", "B"), "x"))
            (Type, Pident (Ident.create_persistent "totoid2"))
     in
     Format.eprintf "\nTRIE %a\n%!" pp t;
     Format.eprintf "TRIESEQ %a\n%!"
       (Format.pp_print_seq pp_lid_paths)
       (to_seq t);
     Format.eprintf "TRIEACHED %a\n%!"
       (Format.pp_print_seq pp_lid_paths)
       (to_seq (reach t (Lapply (Lident "F", Lident "A")) |> Option.get)) *)
end

type t = Lid_trie.t
let empty = Lid_trie.empty
let add = Lid_trie.add
let union = Lid_trie.union
let singleton lid path = Lid_trie.trie_of_lid lid (Paths.singleton path)

let pp = Lid_trie.pp
