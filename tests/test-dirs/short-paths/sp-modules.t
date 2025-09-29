We get a bad path for `hello`

  $ cat > foo.ml <<EOF
  > module type S = sig type t end
  > module Bar : S = struct
  >   type t = int
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
  Define type t/281[1]
  # 0.01 discourse - def
  Define type t/283[4]
  # 0.01 discourse - use
  Use type int/1! File "foo.ml", line 3, characters 11-14
  # 0.01 discourse - def
  Define module Bar/284[3]
  # 0.01 discourse - use
  Use module Bar/284[3] File "foo.ml", line 5, characters 13-16
  # 0.01 discourse - def
  Define module Foo/285[5]
  # 0.01 discourse - def
  Define type u/286[6]
  # 0.01 discourse - use
  Use type Foo/285[5].t File "foo.ml", line 6, characters 9-14
  # 0.01 discourse - use
  Use module Foo/285[5] File "foo.ml", line 6, characters 9-14
  # 0.01 discourse - discourse
  t/281[1]; u/286[6]; Bar/284[3]; Foo/285[5]; int/1!;
  Foo/285[5].t
  {
    "class": "return",
    "value": [],
    "notifications": []
  }
