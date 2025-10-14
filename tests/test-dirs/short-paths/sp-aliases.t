  $ cat >test.ml <<'EOF'
  > type t = Foo
  > module X = struct
  >   type prev = t
  >   type t = Bar
  >   let err (_x : prev) = (Bar : t)
  > end
  > EOF

  $ echo "FLG -short-paths" > .merlin

  $ $MERLIN single type-enclosing -position 5:7 \
  > -log-file - -log-section discourse,short-paths -filename test.ml < test.ml 
  # 0.01 discourse - def
  Define type t/281[1]
  # 0.02 discourse - def
  Define type prev/283[3]
  # 0.02 discourse - use
  Use type t/281[1] File "test.ml", line 3, characters 14-15
  # 0.02 discourse - def
  Define type t/284[4]
  # 0.02 discourse - use
  Use type prev/283[3] File "test.ml", line 5, characters 16-20
  # 0.02 discourse - use
  Use type t/284[4] File "test.ml", line 5, characters 31-32
  # 0.02 discourse - def
  Define module X/289[2]
  # 0.02 discourse - discourse
  X/289[2]; t/281[1]; prev/283[3]; X/289[2].t; X/289[2].err; X/289[2].prev
  # 0.02 short-paths - find_type_simple
  Initial: prev/283[3]
  # 0.02 short-paths - find_type_simple
  Canon: t/281[1]
  # 0.02 short-paths - fill_map
  Treating t/281[1]
  # 0.02 short-paths - fill_map
  Treating prev/283[3]
  # 0.02 short-paths - find_type_simple
  Short: prev/283[3]
  # 0.02 short-paths - find_type_simple
  Initial: t/284[4]
  # 0.02 short-paths - find_type_simple
  Canon: t/284[4]
  # 0.02 short-paths - fill_map
  Treating t/281[1]
  # 0.02 short-paths - fill_map
  Treating prev/283[3]
  # 0.02 short-paths - fill_map
  Treating X/289[2].t
  # 0.02 short-paths - fill_map
  Treating X/289[2].prev
  # 0.02 short-paths - fill_map
  Empty queue
  # 0.02 short-paths - find_type_simple
  Short: t/284[4]
  # 0.02 short-paths - find_type_simple
  Initial: t/281[1]
  # 0.02 short-paths - find_type_simple
  Canon: t/281[1]
  # 0.02 short-paths - fill_map
  Treating t/281[1]
  # 0.02 short-paths - find_type_simple
  Short: t/281[1]
  # 0.02 short-paths - find_type_simple
  Initial: prev/283[3]
  # 0.02 short-paths - find_type_simple
  Canon: t/281[1]
  # 0.02 short-paths - fill_map
  Treating t/281[1]
  # 0.02 short-paths - fill_map
  Treating prev/283[3]
  # 0.02 short-paths - find_type_simple
  Short: prev/283[3]
  # 0.02 short-paths - find_type_simple
  Initial: t/284[4]
  # 0.02 short-paths - find_type_simple
  Canon: t/284[4]
  # 0.02 short-paths - fill_map
  Treating t/281[1]
  # 0.02 short-paths - fill_map
  Treating prev/283[3]
  # 0.02 short-paths - fill_map
  Treating X/289[2].t
  # 0.02 short-paths - fill_map
  Treating X/289[2].prev
  # 0.02 short-paths - fill_map
  Empty queue
  # 0.02 short-paths - find_type_simple
  Short: t/284[4]
  # 0.02 short-paths - find_type_simple
  Initial: t/281[1]
  # 0.02 short-paths - find_type_simple
  Canon: t/281[1]
  # 0.02 short-paths - fill_map
  Treating t/281[1]
  # 0.02 short-paths - find_type_simple
  Short: t/281[1]
  # 0.02 short-paths - find_type_simple
  Initial: prev/283[3]
  # 0.02 short-paths - find_type_simple
  Canon: t/281[1]
  # 0.02 short-paths - fill_map
  Treating t/281[1]
  # 0.02 short-paths - fill_map
  Treating prev/283[3]
  # 0.02 short-paths - find_type_simple
  Short: prev/283[3]
  # 0.02 short-paths - find_type_simple
  Initial: t/284[4]
  # 0.02 short-paths - find_type_simple
  Canon: t/284[4]
  # 0.02 short-paths - fill_map
  Treating t/281[1]
  # 0.02 short-paths - fill_map
  Treating prev/283[3]
  # 0.02 short-paths - fill_map
  Treating X/289[2].t
  # 0.02 short-paths - fill_map
  Treating X/289[2].prev
  # 0.02 short-paths - fill_map
  Empty queue
  # 0.02 short-paths - find_type_simple
  Short: t/284[4]
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 5,
          "col": 6
        },
        "end": {
          "line": 5,
          "col": 9
        },
        "type": "prev -> t",
        "tail": "no"
      },
      {
        "start": {
          "line": 2,
          "col": 11
        },
        "end": {
          "line": 6,
          "col": 3
        },
        "type": "sig type prev = t type t = Bar val err : prev -> t end",
        "tail": "no"
      },
      {
        "start": {
          "line": 2,
          "col": 0
        },
        "end": {
          "line": 6,
          "col": 3
        },
        "type": "sig type prev = t type t = Bar val err : prev -> t end",
        "tail": "no"
      }
    ],
    "notifications": []
  }
