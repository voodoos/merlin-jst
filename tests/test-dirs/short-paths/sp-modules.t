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

  $ $MERLIN single type-enclosing -position 8:14 \
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
  # 0.01 short-paths - find_type_simple
  Initial: Bar/289[5].t
  # 0.01 short-paths - normalize_type_path
  Calling [Env.normalize_type_path] for Bar/289[5].t
  # 0.01 short-paths - find_type_simple
  Canon: Bar/289[5].t
  # 0.01 short-paths - normalize_type_path
  Calling [Env.normalize_type_path] for t/282[2]
  # 0.01 short-paths - fill_by_level
  Treating t/282[2] (t/282[2])
  # 0.01 short-paths - normalize_type_path
  Calling [Env.normalize_type_path] for t/285[6]
  # 0.01 short-paths - fill_by_level
  Treating t/285[6] (t/285[6])
  # 0.01 short-paths - normalize_type_path
  Found type expansion u/291[10] for u/291[10]
  # 0.01 short-paths - fill_by_level
  Treating u/291[10] (u/291[10])
  # 0.01 short-paths - normalize_type_path
  Calling [Env.normalize_type_path] for int/1!
  # 0.01 short-paths - fill_by_level
  Treating int/1! (int/1!)
  # 0.01 short-paths - normalize_type_path
  Calling [Env.normalize_type_path] for Bar/289[5].t
  # 0.01 short-paths - fill_by_level
  Treating Bar/289[5].t (Bar/289[5].t)
  # 0.01 short-paths - normalize_type_path
  Found type expansion Bar/289[5].t for Foo/290[9].t
  # 0.01 short-paths - normalize_type_path
  Calling [Env.normalize_type_path] for Bar/289[5].t
  # 0.01 short-paths - fill_by_level
  Treating Foo/290[9].t (Bar/289[5].t)
  # 0.01 short-paths - fill_by_level
  Empty queue
  # 0.01 short-paths - normalize_type_path
  Found type expansion Bar/289[5].t for Foo/290[9].t
  # 0.01 short-paths - normalize_type_path
  Calling [Env.normalize_type_path] for Bar/289[5].t
  # 0.01 short-paths - find_type_simple
  Short: Foo/290[9].t
  # 0.01 discourse - discourse
   Empty/281[1] -> [M/286[7]];
  Bar/289[5] -> [Foo/290[9]]
  # 0.01 short-paths - find_type_simple
  Initial: Foo/290[9].t
  # 0.01 short-paths - normalize_type_path
  Found type expansion Bar/289[5].t for Foo/290[9].t
  # 0.01 short-paths - normalize_type_path
  Calling [Env.normalize_type_path] for Bar/289[5].t
  # 0.01 short-paths - find_type_simple
  Canon: Bar/289[5].t
  # 0.01 short-paths - normalize_type_path
  Found type expansion Bar/289[5].t for Foo/290[9].t
  # 0.01 short-paths - normalize_type_path
  Calling [Env.normalize_type_path] for Bar/289[5].t
  # 0.01 short-paths - normalize_type_path
  Calling [Env.normalize_type_path] for t/282[2]
  # 0.01 short-paths - fill_by_level
  Treating t/282[2] (t/282[2])
  # 0.01 short-paths - normalize_type_path
  Calling [Env.normalize_type_path] for t/285[6]
  # 0.01 short-paths - fill_by_level
  Treating t/285[6] (t/285[6])
  # 0.01 short-paths - normalize_type_path
  Found type expansion u/291[10] for u/291[10]
  # 0.01 short-paths - fill_by_level
  Treating u/291[10] (u/291[10])
  # 0.01 short-paths - normalize_type_path
  Found type expansion Bar/289[5].t for Foo/290[9].t
  # 0.01 short-paths - normalize_type_path
  Calling [Env.normalize_type_path] for Bar/289[5].t
  # 0.01 short-paths - normalize_type_path
  Calling [Env.normalize_type_path] for int/1!
  # 0.01 short-paths - fill_by_level
  Treating int/1! (int/1!)
  # 0.01 short-paths - normalize_type_path
  Found type expansion Bar/289[5].t for Foo/290[9].t
  # 0.01 short-paths - normalize_type_path
  Calling [Env.normalize_type_path] for Bar/289[5].t
  # 0.01 short-paths - normalize_type_path
  Calling [Env.normalize_type_path] for Bar/289[5].t
  # 0.01 short-paths - fill_by_level
  Treating Bar/289[5].t (Bar/289[5].t)
  # 0.01 short-paths - normalize_type_path
  Found type expansion Bar/289[5].t for Foo/290[9].t
  # 0.01 short-paths - normalize_type_path
  Calling [Env.normalize_type_path] for Bar/289[5].t
  # 0.01 short-paths - fill_by_level
  Treating Foo/290[9].t (Bar/289[5].t)
  # 0.01 short-paths - fill_by_level
  Empty queue
  # 0.01 short-paths - normalize_type_path
  Found type expansion Bar/289[5].t for Foo/290[9].t
  # 0.01 short-paths - normalize_type_path
  Calling [Env.normalize_type_path] for Bar/289[5].t
  # 0.01 short-paths - find_type_simple
  Short: Foo/290[9].t
  # 0.01 short-paths - find_type_simple
  Initial: Foo/290[9].t
  # 0.01 short-paths - normalize_type_path
  Found type expansion Bar/289[5].t for Foo/290[9].t
  # 0.01 short-paths - normalize_type_path
  Calling [Env.normalize_type_path] for Bar/289[5].t
  # 0.01 short-paths - find_type_simple
  Canon: Bar/289[5].t
  # 0.01 short-paths - normalize_type_path
  Found type expansion Bar/289[5].t for Foo/290[9].t
  # 0.01 short-paths - normalize_type_path
  Calling [Env.normalize_type_path] for Bar/289[5].t
  # 0.01 short-paths - normalize_type_path
  Calling [Env.normalize_type_path] for t/282[2]
  # 0.01 short-paths - fill_by_level
  Treating t/282[2] (t/282[2])
  # 0.01 short-paths - normalize_type_path
  Calling [Env.normalize_type_path] for t/285[6]
  # 0.01 short-paths - fill_by_level
  Treating t/285[6] (t/285[6])
  # 0.01 short-paths - normalize_type_path
  Found type expansion Foo/290[9].t for u/291[10]
  # 0.01 short-paths - normalize_type_path
  Found type expansion Bar/289[5].t for Foo/290[9].t
  # 0.01 short-paths - normalize_type_path
  Calling [Env.normalize_type_path] for Bar/289[5].t
  # 0.01 short-paths - fill_by_level
  Treating u/291[10] (Bar/289[5].t)
  # 0.01 short-paths - normalize_type_path
  Found type expansion Bar/289[5].t for Foo/290[9].t
  # 0.01 short-paths - normalize_type_path
  Calling [Env.normalize_type_path] for Bar/289[5].t
  # 0.01 short-paths - normalize_type_path
  Found type expansion Foo/290[9].t for u/291[10]
  # 0.01 short-paths - normalize_type_path
  Found type expansion Bar/289[5].t for Foo/290[9].t
  # 0.01 short-paths - normalize_type_path
  Calling [Env.normalize_type_path] for Bar/289[5].t
  # 0.01 short-paths - fill_by_level
  Finished level and found a path shorter than the previous level:
   u/291[10]
  # 0.01 short-paths - find_type_simple
  Short: u/291[10]
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
        "type": "type t = Foo.t",
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
        "type": "Foo.t",
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
        "type": "type u = u",
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
  Define module A/285[1]
  # 0.01 discourse - use
  Use module A/285[1].B File "substs.ml", line 2, characters 11-14
  # 0.01 discourse - use
  Use module A/285[1] File "substs.ml", line 2, characters 11-14
  # 0.01 discourse - def
  Define module N/288[5]
  # 0.01 discourse - use
  Use module N/288[5].C File "substs.ml", line 3, characters 11-14
  # 0.01 discourse - use
  Use module N/288[5] File "substs.ml", line 3, characters 11-14
  # 0.01 discourse - def
  Define module M/289[6]
  # 0.01 discourse - use
  Use type A/285[1].B.C.t File "substs.ml", line 4, characters 8-15
  # 0.01 discourse - use
  Use module A/285[1].B.C File "substs.ml", line 4, characters 8-15
  # 0.01 discourse - use
  Use module A/285[1].B File "substs.ml", line 4, characters 8-15
  # 0.01 discourse - use
  Use module A/285[1] File "substs.ml", line 4, characters 8-15
  # 0.01 discourse - use
  Use type A/285[1].B.C.t File "substs.ml", line 4, characters 8-15
  # 0.01 discourse - use
  Use module A/285[1].B.C File "substs.ml", line 4, characters 8-15
  # 0.01 discourse - use
  Use module A/285[1].B File "substs.ml", line 4, characters 8-15
  # 0.01 discourse - use
  Use module A/285[1] File "substs.ml", line 4, characters 8-15
  # 0.01 discourse - discourse
  t/281[4]; C/282[3]; B/283[2]; A/285[1]; N/288[5]; M/289[6]; C/282[3].t;
  B/283[2].C; A/285[1].B; N/288[5].C; B/283[2].C.t; A/285[1].B.C;
  A/285[1].B.C.t A/285[1].B -> [N/288[5]]; N/288[5].C -> [M/289[6]];
  A/285[1].B.C -> [N/288[5].C]
  # 0.01 short-paths - find_type_simple
  Initial: A/285[1].B.C.t
  # 0.01 short-paths - normalize_type_path
  Calling [Env.normalize_type_path] for A/285[1].B.C.t
  # 0.01 short-paths - find_type_simple
  Canon: A/285[1].B.C.t
  # 0.01 short-paths - normalize_type_path
  Calling [Env.normalize_type_path] for t/281[4]
  # 0.01 short-paths - fill_by_level
  Treating t/281[4] (t/281[4])
  # 0.01 short-paths - normalize_type_path
  Calling [Env.normalize_type_path] for C/282[3].t
  # 0.01 short-paths - fill_by_level
  Treating C/282[3].t (C/282[3].t)
  # 0.01 short-paths - normalize_type_path
  Found type expansion A/285[1].B.C.t for M/289[6].t
  # 0.01 short-paths - normalize_type_path
  Calling [Env.normalize_type_path] for A/285[1].B.C.t
  # 0.01 short-paths - fill_by_level
  Treating M/289[6].t (A/285[1].B.C.t)
  # 0.01 short-paths - normalize_type_path
  Found type expansion A/285[1].B.C.t for M/289[6].t
  # 0.01 short-paths - normalize_type_path
  Calling [Env.normalize_type_path] for A/285[1].B.C.t
  # 0.01 short-paths - fill_by_level
  Finished level and found a path shorter than the previous level:
   M/289[6].t
  # 0.01 short-paths - find_type_simple
  Short: M/289[6].t
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

  $ cat > open.ml <<EOF
  > module A = struct type a = int end
  > open A 
  > let x = (5 : a)
  > EOF

FIXME: the canon form of a should be int ?
  $ $MERLIN single type-enclosing -position 3:4 \
  > -log-file - -log-section discourse,short-paths -filename open.ml <open.ml 
  # 0.01 discourse - def
  Define type a/281[2]
  # 0.01 discourse - use
  Use type int/1! File "open.ml", line 1, characters 27-30
  # 0.01 discourse - def
  Define module A/282[1]
  # 0.01 discourse - use
  Use module A/282[1] File "open.ml", line 2, characters 5-6
  # 0.01 discourse - use
  Use type A/282[1].a File "open.ml", line 3, characters 13-14
  # 0.01 discourse - use
  Use module A/282[1] File "open.ml", line 3, characters 13-14
  # 0.01 discourse - discourse
  int/1!; a/281[2]; A/282[1]; A/282[1].a
  # 0.01 short-paths - find_type_simple
  Initial: A/282[1].a
  # 0.01 short-paths - normalize_type_path
  Found type expansion int/1! for A/282[1].a
  # 0.01 short-paths - normalize_type_path
  Calling [Env.normalize_type_path] for int/1!
  # 0.01 short-paths - find_type_simple
  Canon: int/1!
  # 0.01 short-paths - normalize_type_path
  Found type expansion int/1! for A/282[1].a
  # 0.01 short-paths - normalize_type_path
  Calling [Env.normalize_type_path] for int/1!
  # 0.01 short-paths - fill_by_level
  Treating a/281[2] (int/1!)
  # 0.01 short-paths - normalize_type_path
  Found type expansion int/1! for A/282[1].a
  # 0.01 short-paths - normalize_type_path
  Calling [Env.normalize_type_path] for int/1!
  # 0.01 short-paths - fill_by_level
  Finished level and found a path shorter than the previous level:
   a/281[2]
  # 0.01 short-paths - find_type_simple
  Short: a/281[2]
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
