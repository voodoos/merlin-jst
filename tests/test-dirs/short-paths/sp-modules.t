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

  $ $MERLIN single type-enclosing -position 7:8 \
  > -log-file - -log-section discourse,short-paths -filename foo.ml < foo.ml 
  # 0.01 discourse - def
  Define module Empty/281[1]
  # 0.01 discourse - def
  Define type t/282[2]
  # 0.01 discourse - def
  Define module M/283[3]
  # 0.01 discourse - def
  Define modtype S/284[4]
  # 0.01 discourse - def
  Define type t/285[6]
  # 0.01 discourse - use
  Use type int/1! File "foo.ml", line 4, characters 11-14
  # 0.01 discourse - use
  Use module Empty/281[1] File "foo.ml", line 5, characters 13-18
  # 0.01 discourse - def
  Define module M/286[7]
  # 0.01 discourse - use
  Use module type S/284[4] File "foo.ml", line 3, characters 13-14
  # 0.01 discourse - use
  Use module Empty/281[1] File "foo.ml", line 3, characters 31-36
  # 0.01 discourse - def
  Define module Bar/289[5]
  # 0.01 discourse - use
  Use module Bar/289[5] File "foo.ml", line 7, characters 13-16
  # 0.01 discourse - def
  Define module Foo/290[9]
  # 0.01 discourse - def
  Define type u/291[10]
  # 0.01 discourse - use
  Use type Foo/290[9].t File "foo.ml", line 8, characters 9-14
  # 0.01 discourse - use
  Use module Foo/290[9] File "foo.ml", line 8, characters 9-14
  # 0.01 discourse - discourse
  M/283[3]; S/284[4]; t/282[2]; u/291[10]; Bar/289[5]; Foo/290[9]; int/1!;
  Empty/281[1];
  Foo/290[9].t
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 7,
          "col": 7
        },
        "end": {
          "line": 7,
          "col": 10
        },
        "type": "(module Foo)",
        "tail": "no"
      },
      {
        "start": {
          "line": 7,
          "col": 0
        },
        "end": {
          "line": 7,
          "col": 16
        },
        "type": "(module Foo)",
        "tail": "no"
      }
    ],
    "notifications": []
  }
