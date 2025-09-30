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
  Empty/281[1]; Foo/290[9].t Empty/281[1] -> [M/286[7]];
  Bar/289[5] -> [Foo/290[9]]
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


  $ cat > substs.ml <<EOF
  > module A = struct module B = struct module C = struct type t end end end
  > module N = A.B
  > module M = N.C
  > let x : A.B.C.t = assert false
  > EOF

  $ $MERLIN single type-enclosing -position 4:4 \
  > -log-file - -log-section discourse,short-paths -filename substs.ml < substs.ml 
  # 0.01 discourse - def
  Define type t/281[4]
  # 0.01 discourse - def
  Define module C/282[3]
  # 0.01 discourse - def
  Define module B/283[2]
  # 0.01 discourse - def
  Define module A/284[1]
  # 0.01 discourse - use
  Use module A/284[1].B File "substs.ml", line 2, characters 11-14
  # 0.01 discourse - use
  Use module A/284[1] File "substs.ml", line 2, characters 11-14
  # 0.01 discourse - def
  Define module N/287[5]
  # 0.01 discourse - use
  Use module N/287[5].C File "substs.ml", line 3, characters 11-14
  # 0.01 discourse - use
  Use module N/287[5] File "substs.ml", line 3, characters 11-14
  # 0.01 discourse - def
  Define module M/288[6]
  # 0.01 discourse - use
  Use type A/284[1].B.C.t File "substs.ml", line 4, characters 8-15
  # 0.01 discourse - use
  Use module A/284[1].B.C File "substs.ml", line 4, characters 8-15
  # 0.01 discourse - use
  Use module A/284[1].B File "substs.ml", line 4, characters 8-15
  # 0.01 discourse - use
  Use module A/284[1] File "substs.ml", line 4, characters 8-15
  # 0.01 discourse - use
  Use type A/284[1].B.C.t File "substs.ml", line 4, characters 8-15
  # 0.01 discourse - use
  Use module A/284[1].B.C File "substs.ml", line 4, characters 8-15
  # 0.01 discourse - use
  Use module A/284[1].B File "substs.ml", line 4, characters 8-15
  # 0.01 discourse - use
  Use module A/284[1] File "substs.ml", line 4, characters 8-15
  # 0.01 discourse - discourse
  A/284[1]; B/283[2]; C/282[3]; M/288[6]; N/287[5]; t/281[4]; A/284[1].B;
  N/287[5].C; A/284[1].B.C; A/284[1].B.C.t A/284[1].B -> [N/287[5]];
  N/287[5].C -> [M/288[6]];
  A/284[1].B.C -> [N/287[5].C]
  # 0.01 short-paths - find_type_simple
  Initial: A/284[1].B.C.t
  # 0.01 short-paths - find_type_simple
  Canon: A/284[1].B.C.t
  # 0.01 short-paths - fill_map
  Treating t/281[4]
  # 0.01 short-paths - fill_map
  Treating M/288[6].t
  # 0.01 short-paths - find_type_simple
  Short: M/288[6].t
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
