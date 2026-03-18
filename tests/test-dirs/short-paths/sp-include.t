  $ cat >shadow_stdlib.ml <<'EOF'
  > include Stdlib
  > EOF

  $ cat >import0.ml <<'EOF'
  > include (
  >   Shadow_stdlib :
  >     module type of struct
  >       include Shadow_stdlib
  >     end)
  > 
  > module Stdlib = struct
  >   include Stdlib
  > 
  >   module Domain = struct
  >     include Stdlib.Domain
  >   end
  > end

  $ $OCAMLC -c shadow_stdlib.ml import0.ml

FIXME: Domain should not be prefixed
  $ $MERLIN single type-enclosing -short-paths -position 12:4 \
  > -filename import0.ml <import0.ml | tr '\n' ' ' | jq '.value[0].type'
  "sig   type 'a t = 'a Shadow_stdlib.Domain.t   val spawn : (unit -> 'a) -> 'a t   val join : 'a t -> 'a   type id = Shadow_stdlib.Domain.id   val get_id : 'a t -> id   val self : unit -> id   external cpu_relax : unit -> unit = \"%cpu_relax\"   val is_main_domain : unit -> bool   val recommended_domain_count : unit -> int   val self_index : unit -> int   val before_first_spawn : (unit -> unit) -> unit   val at_exit : (unit -> unit) -> unit   module DLS = Domain.DLS   module TLS = Domain.TLS   module Safe = Domain.Safe end"
