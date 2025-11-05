We get a bad path for `hello`

  $ cat > foo.ml <<EOF
  > module Bar = struct
  >   type t = int
  > end
  > module Foo = Bar
  > type u = Foo.t
  > let x : Foo.t = 42
  > let _ = x
  > let x2 : u = 42
  > let _ = x2
  > EOF

$ $MERLIN single dump -what parsetree -filename foo.ml < foo.ml

  $ $MERLIN single errors -filename foo.ml < foo.ml  
  {
    "class": "return",
    "value": [],
    "notifications": []
  }

  $ echo "FLG -short-paths" > .merlin
> -log-file - -log-section discourse,short-paths \
  $ $MERLIN single type-enclosing -position 7:8 \
  > -filename foo.ml < foo.ml 
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 7,
          "col": 8
        },
        "end": {
          "line": 7,
          "col": 9
        },
        "type": "u",
        "tail": "no"
      },
      {
        "start": {
          "line": 7,
          "col": 8
        },
        "end": {
          "line": 7,
          "col": 9
        },
        "type": "u",
        "tail": "no"
      }
    ],
    "notifications": []
  }

  $ $MERLIN single type-enclosing -position 9:9 \
  > -filename foo.ml < foo.ml 
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 9,
          "col": 8
        },
        "end": {
          "line": 9,
          "col": 10
        },
        "type": "u",
        "tail": "no"
      },
      {
        "start": {
          "line": 9,
          "col": 8
        },
        "end": {
          "line": 9,
          "col": 10
        },
        "type": "u",
        "tail": "no"
      }
    ],
    "notifications": []
  }

$ $MERLIN single type-enclosing -position 13:5 -filename foo.ml < foo.ml | jq .value[0].type -r
Bar.Foo.t

It seems to be related to shadowing somehow. This works:
  $ cat > foo.ml <<EOF
  > module Foo = struct
  >   type t
  > end
  > 
  > module Bar = struct
  >   module Baz = struct
  >     type t = Foo.t
  >   end
  > end
  > 
  > open! Bar
  > 
  > let hello : Baz.t = 0
  > EOF

$ $MERLIN single type-enclosing -position 13:5 -filename foo.ml < foo.ml | jq .value[0].type -r
Foo.t
