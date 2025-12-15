
  $ cat >base__.ml <<'EOF'
  > module Or_error = Base__Or_error
  > EOF

  $ cat >or_error.ml <<'EOF'
  > type 'a t = ('a,'a) Result.t
  > let ok (x : 'a) : 'a t = Ok x
  > EOF

  $ cat >test.ml <<'EOF'
  > let x = Or_error.ok 42
  > EOF

  $ $OCAMLC -c -o Base__Or_error or_error.ml

  $ $OCAMLC -c -nopervasives -nostdlib -no-alias-deps base__.ml

  $ $OCAMLC -c -no-alias-deps -open Base__ test.ml

  $ ls
  Base__Or_error.cmi
  Base__Or_error.cmo
  base__.cmi
  base__.cmo
  base__.ml
  or_error.ml
  test.cmi
  test.cmo
  test.ml

  $ cat >.merlin <<'EOF'
  > FLG -short-paths -open Base__
  > EOF

  $ $MERLIN single type-enclosing -position 1:4 \
  > -filename test.ml < test.ml | jq '.value[0].type'
  "int Base__.Or_error.t"
