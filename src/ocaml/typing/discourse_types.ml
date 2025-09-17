(* This module exists to prevent a dependency cycle with Types. *)
module Paths = struct
  module T = struct
    type t = Shape.Sig_component_kind.t * Path.t

    let rec length acc = function
      | Path.Pident _ -> acc + 1
      | Pdot (p, _) | Papply (p, _) | Pextra_ty (p, _) -> length (acc + 1) p

    let length (_, p) = length 0 p

    let compare p1 p2 = if p1 == p2 then 0 else length p1 - length p2
  end

  include Set.Make (T)
end

let empty = Paths.empty
