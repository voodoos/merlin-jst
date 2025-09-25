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
  Add type t/281[2]
  Use type int/1!
  Add type u/284[4]
  Use type Foo/283[3].t
  Use type Foo/283[3].t
  Use type Foo/283[3].t
  Use type u/284[4]
  Use type u/284[4]
  {
    "class": "return",
    "value": [],
    "notifications": []
  }

  $ echo "FLG -short-paths" > .merlin

  $ $MERLIN single type-enclosing -position 7:8 \
  > -log-file - -log-section discourse -filename foo.ml < foo.ml 
  Add type t/281[2]
  Use type int/1!
  Add type u/284[4]
  Use type Foo/283[3].t
  Use type Foo/283[3].t
  Use type Foo/283[3].t
  Use type u/284[4]
  Use type u/284[4]
  Find type simple: Foo/283[3].t
  Find type simple canon: int/1!
  PQ t/281[2] 
  PQ u/284[4] 
  PQ int/1! 
  PQ Foo/283[3].t 
  Treating t/281[2]
  Treating u/284[4]
  Find type simple short: u/284[4]
  # 0.01 discourse - discourse
  t/281[2]; u/284[4]; Bar/282[1]; Foo/283[3]; int/1!;
  Foo/283[3].t
  Find type simple: Foo/283[3].t
  Find type simple canon: int/1!
  PQ t/281[2] 
  PQ u/284[4] 
  PQ int/1! 
  PQ Foo/283[3].t 
  Treating t/281[2]
  Treating u/284[4]
  Find type simple short: u/284[4]
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

  $ $MERLIN single type-enclosing -position 9:9 -filename foo.ml < foo.ml 
  Add type t/281[2]
  Use type int/1!
  Add type u/284[4]
  Use type Foo/283[3].t
  Use type Foo/283[3].t
  Use type Foo/283[3].t
  Use type u/284[4]
  Use type u/284[4]
  Find type simple: u/284[4]
  Find type simple canon: int/1!
  PQ t/281[2] 
  PQ u/284[4] 
  PQ int/1! 
  PQ Foo/283[3].t 
  Treating t/281[2]
  Treating u/284[4]
  Find type simple short: u/284[4]
  Find type simple: u/284[4]
  Find type simple canon: int/1!
  PQ t/281[2] 
  PQ u/284[4] 
  PQ int/1! 
  PQ Foo/283[3].t 
  Treating t/281[2]
  Treating u/284[4]
  Find type simple short: u/284[4]
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
