  $ echo "FLG -short-paths" > .merlin

  $ cat >quite_long__name.ml <<'EOF'
  > module M = struct type t = T let x = T end 
  > EOF

  $ cat >import0.ml <<'EOF'
  > module N = Quite_long__name.M
  > EOF

  $ cat >import.ml <<'EOF'
  > include Import0
  > EOF

  $ cat >test.ml <<'EOF'
  > open Import;;
  > ignore N.x
  > EOF

  $ $OCAMLC -c quite_long__name.ml import0.ml import.ml

Should take the alias in Import into account. Should be N.t.
  $ $MERLIN single type-enclosing -position 2:9 \
  > -filename test.ml < test.ml | jq '.value[0].type'
  "N.t"
