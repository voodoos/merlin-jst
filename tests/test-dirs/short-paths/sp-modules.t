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
        "type": "M/2.t",
        "tail": "no"
      }
    ],
    "notifications": []
  }


  $ cat > substs.ml <<EOF
  > module W = struct
  >   module A = struct module B = struct module C = struct type t end end end
  >   module N = A.B
  >   module M = N.C
  > end
  > let x : W.A.B.C.t = assert false
  > open W 
  > let x : A.B.C.t = assert false
  > EOF

$ $MERLIN single type-enclosing -position 6:4 \
> -log-file - -log-section short-paths -filename substs.ml < substs.ml 

  $ $MERLIN single type-enclosing -position 8:4 \
  > -log-file - -log-section discourse,short-paths -filename substs.ml < substs.ml
  # 0.01 discourse - def
  Define type t/281[5]
  # 0.01 discourse - def
  Define module C/282[4]
  # 0.01 discourse - def
  Define module B/283[3]
  # 0.01 discourse - def
  Define module A/285[2]
  # 0.01 discourse - use
  Use module A/285[2].B File "substs.ml", line 3, characters 13-16
  # 0.01 discourse - use
  Use module A/285[2] File "substs.ml", line 3, characters 13-16
  # 0.01 discourse - def
  Define module N/288[6]
  # 0.01 discourse - use
  Use module N/288[6].C File "substs.ml", line 4, characters 13-16
  # 0.01 discourse - use
  Use module N/288[6] File "substs.ml", line 4, characters 13-16
  # 0.01 discourse - def
  Define module M/289[7]
  # 0.01 discourse - def
  Define module W/290[1]
  # 0.01 discourse - use
  Use type W/290[1].A.B.C.t File "substs.ml", line 6, characters 8-17
  # 0.01 discourse - use
  Use module W/290[1].A.B.C File "substs.ml", line 6, characters 8-17
  # 0.01 discourse - use
  Use module W/290[1].A.B File "substs.ml", line 6, characters 8-17
  # 0.01 discourse - use
  Use module W/290[1].A File "substs.ml", line 6, characters 8-17
  # 0.01 discourse - use
  Use module W/290[1] File "substs.ml", line 6, characters 8-17
  # 0.01 discourse - use
  Use type W/290[1].A.B.C.t File "substs.ml", line 6, characters 8-17
  # 0.01 discourse - use
  Use module W/290[1].A.B.C File "substs.ml", line 6, characters 8-17
  # 0.01 discourse - use
  Use module W/290[1].A.B File "substs.ml", line 6, characters 8-17
  # 0.01 discourse - use
  Use module W/290[1].A File "substs.ml", line 6, characters 8-17
  # 0.01 discourse - use
  Use module W/290[1] File "substs.ml", line 6, characters 8-17
  # 0.01 discourse - use
  Use module W/290[1] File "substs.ml", line 7, characters 5-6
  # 0.01 discourse - use
  Use type W/290[1].A.B.C.t File "substs.ml", line 8, characters 8-15
  # 0.01 discourse - use
  Use module W/290[1].A.B.C File "substs.ml", line 8, characters 8-15
  # 0.01 discourse - use
  Use module W/290[1].A.B File "substs.ml", line 8, characters 8-15
  # 0.01 discourse - use
  Use module W/290[1].A File "substs.ml", line 8, characters 8-15
  # 0.01 discourse - use
  Use type W/290[1].A.B.C.t File "substs.ml", line 8, characters 8-15
  # 0.01 discourse - use
  Use module W/290[1].A.B.C File "substs.ml", line 8, characters 8-15
  # 0.01 discourse - use
  Use module W/290[1].A.B File "substs.ml", line 8, characters 8-15
  # 0.01 discourse - use
  Use module W/290[1].A File "substs.ml", line 8, characters 8-15
  # 0.01 discourse - discourse
  Root :> A: (A,[A/285[2];
            W/290[1].A]) :> B: (A.B,[A/285[2].B;
                              W/290[1].A.B]) :> C: (A.B.C,[A/285[2].B.C;
                                                  W/290[1].A.B.C]) :> t: (A.B.C.t,[A/285[2].B.C.t;
                                                                      W/290[1].A.B.C.t]) :> 
  B: (B,[B/283[3]]) :> C: (B.C,[B/283[3].C]) :> t: (B.C.t,[B/283[3].C.t]) :> 
  C: (C,[C/282[4]]) :> t: (C.t,[C/282[4].t]) :> M: (M,[M/289[7]]) :> 
  N: (N,[N/288[6]]) :> C: (N.C,[N/288[6].C]) :> 
  W: (W,[W/290[1]]) :> A: (W.A,[W/290[1].A]) :> B: (W.A.B,[W/290[1].A.B]) :> 
                                                  C: (W.A.B.C,[W/290[1].A.B.C]) :> 
                                                    t: (W.A.B.C.t,[W/290[1].A.B.C.t]) :> 
    M: (W.M,[W/290[1].M]) :> 
    N: (W.N,[W/290[1].N]) :> t: (t,[t/281[5]]) :>  A/285[2].B -> [N/288[6]];
  N/288[6].C -> [M/289[7]]; A/285[2].B.C -> [N/288[6].C];
  W/290[1].A.B -> [W/290[1].N];
  W/290[1].N.C -> [W/290[1].M]
  # 0.01 short-paths - find_type_simple
  Initial: W/290[1].A.B.C.t
  # 0.01 short-paths - normalize_type_path
  Calling [Env.normalize_type_path] for W/290[1].A.B.C.t
  # 0.01 short-paths - find_type_simple
  Canon: W/290[1].A.B.C.t
  # 0.01 short-paths - fill_by_level
  Current best: 
  # 0.01 short-paths - fill_by_level
  Treating t (t/281[5])
  # 0.01 short-paths - fill_by_level
  Name: t invalid in the current env
  # 0.01 short-paths - fill_by_level
  Current best: 
  # 0.01 short-paths - fill_by_level
  Finished a level. Current best: 
  # 0.01 short-paths - fill_by_level
  Treating C.t (C/282[4].t)
  # 0.01 short-paths - fill_by_level
  Name: C.t invalid in the current env
  # 0.01 short-paths - fill_by_level
  Current best: 
  # 0.01 short-paths - fill_by_level
  Finished a level. Current best: 
  # 0.01 short-paths - fill_by_level
  Treating M.t (A/285[2].B.C.t)
  # 0.01 short-paths - fill_by_level
  find_type_by_name M.t (A/285[2].B.C.t) = W/290[1].M.t
  # 0.01 short-paths - normalize_type_path
  Calling [Env.normalize_type_path] for A/285[2].B.C.t
  # 0.01 short-paths - normalize_type_path
  Found type expansion W/290[1].A.B.C.t for W/290[1].M.t
  # 0.01 short-paths - normalize_type_path
  Calling [Env.normalize_type_path] for W/290[1].A.B.C.t
  # 0.01 short-paths - fill_by_level
  find_type_by_name A/285[2].B.C.t <>? W/290[1].A.B.C.t
  # 0.01 short-paths - fill_by_level
  Name: M.t invalid in the current env
  # 0.01 short-paths - fill_by_level
  Current best: 
  # 0.01 short-paths - fill_by_level
  Treating M.t (W/290[1].A.B.C.t)
  # 0.01 short-paths - fill_by_level
  find_type_by_name M.t (W/290[1].A.B.C.t) = W/290[1].M.t
  # 0.01 short-paths - normalize_type_path
  Calling [Env.normalize_type_path] for W/290[1].A.B.C.t
  # 0.01 short-paths - normalize_type_path
  Found type expansion W/290[1].A.B.C.t for W/290[1].M.t
  # 0.01 short-paths - normalize_type_path
  Calling [Env.normalize_type_path] for W/290[1].A.B.C.t
  # 0.01 short-paths - fill_by_level
  find_type_by_name W/290[1].A.B.C.t <>? W/290[1].A.B.C.t
  # 0.01 short-paths - normalize_type_path
  Calling [Env.normalize_type_path] for W/290[1].A.B.C.t
  # 0.01 short-paths - fill_by_level
  Found canonical path W/290[1].A.B.C.t
  # 0.01 short-paths - fill_by_level
  Current best: 
  # 0.01 short-paths - normalize_type_path
  Calling [Env.normalize_type_path] for W/290[1].A.B.C.t
  # 0.01 short-paths - normalize_type_path
  Found type expansion W/290[1].A.B.C.t for W/290[1].M.t
  # 0.01 short-paths - normalize_type_path
  Calling [Env.normalize_type_path] for W/290[1].A.B.C.t
  # 0.01 short-paths - fill_by_level
  Finished level and found a name shorter than the previous level:
   M.t
  # 0.01 short-paths - shorten
  Masking path W/290[1].A.B.C.t with lid M.t
  # 0.01 short-paths - find_type_simple
  Short: M/385[1].t
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 8,
          "col": 4
        },
        "end": {
          "line": 8,
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

Open + Subst

$ cat >open.ml <<EOF
> module A = struct module B = struct module C = struct type a = V end end end
> module X = A.B.C
> open A.B.C
> let x = (V : X.a)
> EOF

$ $MERLIN single type-enclosing -position 4:4 \
> -log-file - -filename open.ml <open.ml 
