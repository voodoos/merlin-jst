This mocks the Async --include--> Async_kernel --exports--> Deferred

  $ mkdir async_kernel

  $ cat >async_kernel/deferred0.ml <<'EOF'
  > type +'a t = 'a
  > let create : 'a -> 'a t = Fun.id
  > EOF

  $ cat >async_kernel/deferred0.mli <<'EOF'
  > type +'a t 
  > val create : 'a -> 'a t
  > EOF

  $ cat >async_kernel/deferred1.ml <<'EOF'
  > type +'a t = 'a Deferred0.t
  > 
  > module Let_syntax = struct 
  >   module Let_syntax = struct let return x = Deferred0.create x end
  > end
  > EOF

  $ cat >async_kernel/deferred.ml <<'EOF'
  > include Deferred1
  > EOF

  $ cat >async_kernel/deferred.mli <<'EOF'
  > type +'a t = 'a Deferred1.t 
  > EOF

  $ cat >async_kernel/async_kernel.ml <<'EOF'
  > module Deferred = Deferred
  > include Deferred1.Let_syntax
  > EOF

  $ mkdir async

  $ cat >async/async.ml <<'EOF'
  > include Async_kernel
  > EOF

  $ cat >test.ml <<'EOF'
  > open! Async
  > 
  > let foo = Let_syntax.return 5
  > EOF

  $ cd async_kernel && $OCAMLC -c \
  > deferred0.mli deferred0.ml deferred1.ml \
  > deferred.mli deferred.ml async_kernel.ml

  $ cd ../async && $OCAMLC -c async.ml -I ../async_kernel

  $ cd .. && $OCAMLC -c test.ml -I async_kernel -I async 

  $ cat >.merlin <<'EOF'
  > FLG -short-paths
  > B .
  > B async
  > B async_kernel
  > EOF

  $ $MERLIN single errors \
  > -filename test.ml < test.ml 
  {
    "class": "return",
    "value": [],
    "notifications": []
  }
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
        "type": "int Deferred0.t",
        "tail": "no"
      }
    ],
    "notifications": []
  }
