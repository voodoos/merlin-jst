We get a bad path for `hello`

  $ cat > foo.ml <<EOF
  > module Empty = struct end
  > module type S = sig type t module M : sig end end
  > module Bar : S with module M = Empty = struct
  >   type t = int
  >   module M = Empty
  > end
  > module Foo = Bar
  > type u = Foo.t
  > EOF

$ $MERLIN single dump -what parsetree -filename foo.ml < foo.ml

  $ $MERLIN single errors -filename foo.ml < foo.ml  
  {
    "class": "return",
    "value": [],
    "notifications": []
  }

  $ echo "FLG -short-paths" > .merlin

> -log-file - -log-section discourse,type-enclosing,short-paths 
  $ $MERLIN single type-enclosing -position 8:14 \
  > -filename foo.ml < foo.ml 
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 8,
          "col": 9
        },
        "end": {
          "line": 8,
          "col": 14
        },
        "type": "type t = Bar.t",
        "tail": "no"
      },
      {
        "start": {
          "line": 8,
          "col": 9
        },
        "end": {
          "line": 8,
          "col": 14
        },
        "type": "Bar.t",
        "tail": "no"
      },
      {
        "start": {
          "line": 8,
          "col": 0
        },
        "end": {
          "line": 8,
          "col": 14
        },
        "type": "type u = Bar.t",
        "tail": "no"
      }
    ],
    "notifications": []
  }


  $ cat > substs.ml <<EOF
  > module A = struct module B = struct module C = struct type t end end end
  > module N = A.B
  > module M = N.C
  > let x : A.B.C.t = assert false
  > EOF

  $ $MERLIN single type-enclosing -position 4:4 \
  > -filename substs.ml < substs.ml 
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 4,
          "col": 4
        },
        "end": {
          "line": 4,
          "col": 5
        },
        "type": "M.t",
        "tail": "no"
      }
    ],
    "notifications": []
  }

  $ cat >open.ml <<EOF
  > module A = struct type a = int end
  > include A 
  > let x = (5 : a)
  > EOF

  $ $MERLIN single type-enclosing -position 3:4 \
  > -filename open.ml <open.ml 
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
          "col": 5
        },
        "type": "a",
        "tail": "no"
      }
    ],
    "notifications": []
  }

  $ cat >open.ml <<EOF
  > module A = struct type a = int end
  > open A 
  > let x = (5 : a)
  > EOF

  $ $MERLIN single type-enclosing -position 3:4 \
  > -filename open.ml <open.ml 
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
          "col": 5
        },
        "type": "a",
        "tail": "no"
      }
    ],
    "notifications": []
  }
