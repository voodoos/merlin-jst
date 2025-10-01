(* This module exists to prevent a dependency cycle with Types. *)
module Paths = struct
  module T = struct
    type t = Shape.Sig_component_kind.t * Path.t

    let compare_strings s1 s2 =
      let ls1 = String.length s1 in
      let ls2 = String.length s2 in
      if ls1 == ls2 then String.compare s1 s2 else ls1 - ls2

    let rec compare_paths (p1 : Path.t) (p2 : Path.t) =
      if p1 == p2 then 0
      else
        match (p1, p2) with
        | Path.Pident id1, Pident id2 ->
          compare_strings (Ident.name id1) (Ident.name id2)
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

    let compare (_, p1) (_, p2) = compare_paths p1 p2
  end

  include Set.Make (T)
end

let empty = Paths.empty

let pp ppf t =
  let pp_sep ppf () = Format.fprintf ppf ";@;" in
  let paths = Paths.elements t |> List.map snd in
  Format.pp_print_list ~pp_sep Path.print ppf paths
