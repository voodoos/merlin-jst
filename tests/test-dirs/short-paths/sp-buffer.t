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

  $ $MERLIN single type-enclosing -position 7:8 \
  > -log-file - -log-section discourse,short-paths -filename foo.ml < foo.ml 
  # 0.02 discourse - def
  Define type t/281[2]
  # 0.02 discourse - use
  Use type int/1! File "foo.ml", line 2, characters 11-14
  # 0.02 discourse - def
  Define module Bar/282[1]
  # 0.02 discourse - use
  Use module Bar/282[1] File "foo.ml", line 4, characters 13-16
  # 0.02 discourse - def
  Define module Foo/283[3]
  # 0.02 discourse - def
  Define type u/284[4]
  # 0.02 discourse - use
  Use type Foo/283[3].t File "foo.ml", line 5, characters 9-14
  # 0.02 discourse - use
  Use module Foo/283[3] File "foo.ml", line 5, characters 9-14
  # 0.02 discourse - use
  Use type Foo/283[3].t File "foo.ml", line 6, characters 8-13
  # 0.02 discourse - use
  Use module Foo/283[3] File "foo.ml", line 6, characters 8-13
  # 0.02 discourse - use
  Use type Foo/283[3].t File "foo.ml", line 6, characters 8-13
  # 0.02 discourse - use
  Use module Foo/283[3] File "foo.ml", line 6, characters 8-13
  # 0.02 discourse - use
  Use value x/285 File "foo.ml", line 7, characters 8-9
  # 0.02 discourse - use
  Use type u/284[4] File "foo.ml", line 8, characters 9-10
  # 0.02 discourse - use
  Use type u/284[4] File "foo.ml", line 8, characters 9-10
  # 0.02 discourse - use
  Use value x2/286 File "foo.ml", line 9, characters 8-10
  # 0.02 discourse - use
  Use value x/285 File "foo.ml", line 1, characters 0-1
  # 0.02 short-paths - find_type_simple
  Initial: Foo/283[3].t
  # 0.02 short-paths - find_type_simple
  Canon: int/1!
  # 0.02 short-paths - fill_by_level
  Treating t/281[2]
  # 0.02 short-paths - fill_by_level
  Treating u/284[4]
  # 0.02 short-paths - fill_by_level
  Finished level and found a path shorter than the previous level:
   u/284[4]
  # 0.02 short-paths - find_type_simple
  Short: u/284[4]
  # 0.02 discourse - discourse
  
  Bar/282[1] -> [Foo/283[3]]
  # 0.02 short-paths - find_type_simple
  Initial: Foo/283[3].t
  # 0.02 short-paths - find_type_simple
  Canon: int/1!
  # 0.02 short-paths - fill_by_level
  Treating t/281[2]
  # 0.02 short-paths - fill_by_level
  Finished level and found a path shorter than the previous level:
   u/284[4]
  # 0.02 short-paths - find_type_simple
  Short: u/284[4]
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
  > -log-file - -log-section discourse,short-paths -filename foo.ml < foo.ml 
  # 0.01 discourse - def
  Define type t/281[2]
  # 0.01 discourse - use
  Use type int/1! File "foo.ml", line 2, characters 11-14
  # 0.01 discourse - def
  Define module Bar/282[1]
  # 0.01 discourse - use
  Use module Bar/282[1] File "foo.ml", line 4, characters 13-16
  # 0.01 discourse - def
  Define module Foo/283[3]
  # 0.01 discourse - def
  Define type u/284[4]
  # 0.01 discourse - use
  Use type Foo/283[3].t File "foo.ml", line 5, characters 9-14
  # 0.01 discourse - use
  Use module Foo/283[3] File "foo.ml", line 5, characters 9-14
  # 0.01 discourse - use
  Use type Foo/283[3].t File "foo.ml", line 6, characters 8-13
  # 0.01 discourse - use
  Use module Foo/283[3] File "foo.ml", line 6, characters 8-13
  # 0.01 discourse - use
  Use type Foo/283[3].t File "foo.ml", line 6, characters 8-13
  # 0.01 discourse - use
  Use module Foo/283[3] File "foo.ml", line 6, characters 8-13
  # 0.01 discourse - use
  Use value x/285 File "foo.ml", line 7, characters 8-9
  # 0.01 discourse - use
  Use type u/284[4] File "foo.ml", line 8, characters 9-10
  # 0.01 discourse - use
  Use type u/284[4] File "foo.ml", line 8, characters 9-10
  # 0.01 discourse - use
  Use value x2/286 File "foo.ml", line 9, characters 8-10
  # 0.01 discourse - use
  Use value x2/286 File "foo.ml", line 1, characters 0-2
  # 0.01 short-paths - find_type_simple
  Initial: u/284[4]
  # 0.01 short-paths - find_type_simple
  Canon: int/1!
  # 0.01 short-paths - fill_by_level
  Treating t/281[2]
  # 0.01 short-paths - fill_by_level
  Treating u/284[4]
  # 0.01 short-paths - fill_by_level
  Finished level and found a path shorter than the previous level:
   u/284[4]
  # 0.01 short-paths - find_type_simple
  Short: u/284[4]
  # 0.01 discourse - discourse
  
  Bar/282[1] -> [Foo/283[3]]
  # 0.01 short-paths - find_type_simple
  Initial: u/284[4]
  # 0.01 short-paths - find_type_simple
  Canon: int/1!
  # 0.01 short-paths - fill_by_level
  Treating t/281[2]
  # 0.01 short-paths - fill_by_level
  Finished level and found a path shorter than the previous level:
   u/284[4]
  # 0.01 short-paths - find_type_simple
  Short: u/284[4]
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
