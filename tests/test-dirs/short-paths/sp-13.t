This mocks the Async --include--> Async_kernel --exports--> Deferred

  $ mkdir async_kernel
  $ cd async_kernel


  $ cat >async_kernel__.ml <<'EOF'
  > module Deferred = Async_kernel__Deferred
  > module Deferred0 = Async_kernel__Deferred0
  > EOF

  $ $OCAMLC -c async_kernel__.ml -no-alias-deps 2>/dev/null
  $ $MERLIN_TEST_OCAML_PATH/bin/ocamlobjinfo -quiet -discourse async_kernel__.cmi
  Discourse:
  Deferred: alias: Async_kernel__Deferred [Async_kernel__Deferred!]
    Async_kernel__Deferred [Async_kernel__Deferred!]
  
  Deferred0: alias: Async_kernel__Deferred0 [Async_kernel__Deferred0!]
    Async_kernel__Deferred0 [Async_kernel__Deferred0!]
  
  $ cat >deferred0.ml <<'EOF'
  > type +'a t = 'a
  > let create : 'a -> 'a t = Fun.id
  > EOF

  $ cat >deferred0.mli <<'EOF'
  > type +'a t 
  > val create : 'a -> 'a t
  > EOF

  $ $OCAMLC -c deferred0.mli -open Async_kernel__ -o Async_kernel__Deferred0
  $ $OCAMLC -c deferred0.ml -open Async_kernel__ -o Async_kernel__Deferred0



  $ cat >deferred.ml <<'EOF'
  > type +'a t = 'a Deferred0.t
  > 
  > module Let_syntax = struct 
  >   module Let_syntax = struct let return x = Deferred0.create x end
  > end
  > EOF

  $ $OCAMLC -c deferred.ml -open Async_kernel__ -o Async_kernel__Deferred


  $ cat >async_kernel.ml <<'EOF'
  > module Deferred = Deferred
  > include Deferred.Let_syntax
  > EOF

  $ $OCAMLC -c async_kernel.ml -open Async_kernel__


  $ cd ..
  $ mkdir async
  $ cd async

  $ cat >async.ml <<'EOF'
  > include Async_kernel
  > EOF

  $ $OCAMLC -c async.ml -I ../async_kernel
  $ $MERLIN_TEST_OCAML_PATH/bin/ocamlobjinfo -quiet -discourse async.cmi
  Discourse:
  Deferred: alias: Async_kernel.Deferred [Async_kernel!.Deferred]
    Deferred [Async_kernel__!.Deferred]
  
  Let_syntax: alias: Async_kernel.Let_syntax [Async_kernel!.Let_syntax] 
  
  $ cd ..

  $ cat >test.ml <<'EOF'
  > open! Async
  > 
  > let foo = Let_syntax.return 5
  > EOF


  $ $OCAMLC -c test.ml  -I async -I async_kernel

  $ cat >.merlin <<'EOF'
  > FLG -short-paths
  > B .
  > B async
  > B async_kernel
  > EOF

  $ $MERLIN single type-enclosing -position 3:5 \
  > -filename test.ml < test.ml 
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 3,
          "col": 4
        },
        "end": {
          "line": 3,
          "col": 7
        },
        "type": "int Deferred.t",
        "tail": "no"
      }
    ],
    "notifications": []
  }
