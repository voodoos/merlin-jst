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
  Add type int/1!
  Add type t/281[2]
  Add type int/1!
  Add type u/284[4]
  Add type Foo/283[3].t
  Add type Foo/283[3].t
  Add type Foo/283[3].t
  Add type u/284[4]
  Add type u/284[4]

  $ echo "FLG -short-paths" > .merlin

  $ $MERLIN single type-enclosing -position 7:8 -filename foo.ml < foo.ml 
  Add type int/1!
  Add type t/281[2]
  Add type int/1!
  Add type u/284[4]
  Add type Foo/283[3].t
  Add type Foo/283[3].t
  Add type Foo/283[3].t
  Add type u/284[4]
  Add type u/284[4]
  Find type simple: Foo/283[3].t
  Find type simple canon: int/1!
  PQ int/1! (1)
  PQ Foo/283[3].t (2)
  Treating int/1!
  Find type simple short: int/1!
  Find type simple: Foo/283[3].t
  Find type simple canon: int/1!
  PQ int/1! (1)
  PQ Foo/283[3].t (2)
  Treating int/1!
  Find type simple short: int/1!

  $ $MERLIN single type-enclosing -position 9:9 -filename foo.ml < foo.ml 
  Add type int/1!
  Add type t/281[2]
  Add type int/1!
  Add type u/284[4]
  Add type Foo/283[3].t
  Add type Foo/283[3].t
  Add type Foo/283[3].t
  Add type u/284[4]
  Add type u/284[4]
  Find type simple: u/284[4]
  Find type simple canon: int/1!
  PQ int/1! (1)
  PQ Foo/283[3].t (2)
  Treating int/1!
  Find type simple short: int/1!
  Find type simple: u/284[4]
  Find type simple canon: int/1!
  PQ int/1! (1)
  PQ Foo/283[3].t (2)
  Treating int/1!
  Find type simple short: int/1!

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
