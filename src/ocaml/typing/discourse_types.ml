(* This module exists to prevent a dependency cycle with Types. *)
module Paths = struct
  module T = struct
    type t = Shape.Sig_component_kind.t * Path.t

    let compare (_, p1) (_, p2) =
      Path.compare p1 p2
  end

  include Set.Make (T)
end

let empty = Paths.empty

let pp ppf t =
  let pp_sep ppf () = Format.fprintf ppf ";@;" in
  let pp_v = Path.print in
  let iter pp_v paths = Paths.iter (fun (_,p) -> pp_v p) paths in
  Format.pp_print_iter ~pp_sep iter pp_v ppf t
