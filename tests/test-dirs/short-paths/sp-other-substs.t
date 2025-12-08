  $ echo "FLG -short-paths" > .merlin

  $ cat >import.ml <<'EOF'
  > module QuiteLongName = struct module M = struct type t = T end end
  > module N = QuiteLongName.M
  > EOF

  $ cat >test.ml <<'EOF'
  > ignore Import.QuiteLongName.M.T
  > EOF

  $ $OCAMLC -c import.ml

FIXME: should take the alias in Import into account.
  $ $MERLIN single type-enclosing -position 1:30 \
  > -filename test.ml < test.ml | jq '.value[0].type'
  "Import.QuiteLongName.M.t"
