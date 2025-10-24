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
  # 0.01 discourse - def
  Define type prev/283[3]
  # 0.01 discourse - use
  Use type t/281[1] File "test.ml", line 3, characters 14-15
  # 0.01 discourse - def
  Define type t/284[4]
  # 0.01 discourse - use
  Use type prev/283[3] File "test.ml", line 5, characters 16-20
  # 0.01 discourse - use
  Use type t/284[4] File "test.ml", line 5, characters 31-32
  # 0.01 discourse - def
  Define module X/289[2]
  # 0.01 discourse - discourse
  t/281[1]; prev/283[3]; t/284[4]; X/289[2]; X/289[2].err; X/289[2].prev;
  X/289[2].t
  # 0.01 short-paths - find_type_simple
  Initial: prev/283[3]
  # 0.01 short-paths - normalize_type_path
  Found type expansion t/281[1] for prev/283[3]
  # 0.01 short-paths - normalize_type_path
  Calling [Env.normalize_type_path] for t/281[1]
  # 0.01 short-paths - find_type_simple
  Canon: t/281[1]
  # 0.01 short-paths - normalize_type_path
  Calling [Env.normalize_type_path] for t/284[4]
  # 0.01 short-paths - fill_by_level
  Treating t/281[1] (t/284[4])
  # 0.01 short-paths - normalize_type_path
  Calling [Env.normalize_type_path] for t/284[4]
  # 0.01 short-paths - fill_by_level
  Treating t/284[4] (t/284[4])
  # 0.01 short-paths - normalize_type_path
  Found type expansion t/281[1] for prev/283[3]
  # 0.01 short-paths - normalize_type_path
  Calling [Env.normalize_type_path] for t/281[1]
  # 0.01 short-paths - fill_by_level
  Treating prev/283[3] (t/281[1])
  # 0.01 short-paths - normalize_type_path
  Found type expansion t/281[1] for prev/283[3]
  # 0.01 short-paths - normalize_type_path
  Calling [Env.normalize_type_path] for t/281[1]
  # 0.01 short-paths - fill_by_level
  Finished level and found a path shorter than the previous level:
   prev/283[3]
  # 0.01 short-paths - find_type_simple
  Short: prev/283[3]
  # 0.01 short-paths - find_type_simple
  Initial: t/284[4]
  # 0.01 short-paths - normalize_type_path
  Calling [Env.normalize_type_path] for t/284[4]
  # 0.01 short-paths - find_type_simple
  Canon: t/284[4]
  # 0.01 short-paths - normalize_type_path
  Calling [Env.normalize_type_path] for t/284[4]
  # 0.01 short-paths - fill_by_level
  Treating t/281[1] (t/284[4])
  # 0.01 short-paths - normalize_type_path
  Calling [Env.normalize_type_path] for t/284[4]
  # 0.01 short-paths - fill_by_level
  Treating t/284[4] (t/284[4])
  # 0.01 short-paths - normalize_type_path
  Calling [Env.normalize_type_path] for t/284[4]
  # 0.01 short-paths - fill_by_level
  Finished level and found a path shorter than the previous level:
   t/284[4]
  # 0.01 short-paths - find_type_simple
  Short: t/284[4]
  # 0.01 short-paths - find_type_simple
  Initial: t/281[1]
  # 0.01 short-paths - normalize_type_path
  Calling [Env.normalize_type_path] for t/281[1]
  # 0.01 short-paths - find_type_simple
  Canon: t/281[1]
  # 0.01 short-paths - normalize_type_path
  Calling [Env.normalize_type_path] for prev/371
  # 0.01 short-paths - normalize_type_path
  Calling [Env.normalize_type_path] for t/281[1]
  # 0.01 short-paths - fill_by_level
  Treating t/281[1] (t/281[1])
  # 0.01 short-paths - normalize_type_path
  Calling [Env.normalize_type_path] for prev/371
  # 0.01 short-paths - normalize_type_path
  Calling [Env.normalize_type_path] for X/289[2].t
  # 0.01 short-paths - fill_by_level
  Treating X/289[2].t (X/289[2].t)
  # 0.01 short-paths - normalize_type_path
  Calling [Env.normalize_type_path] for prev/371
  # 0.01 short-paths - normalize_type_path
  Calling [Env.normalize_type_path] for X/289[2].prev
  # 0.01 short-paths - fill_by_level
  Treating X/289[2].prev (X/289[2].prev)
  # 0.01 short-paths - fill_by_level
  Empty queue
  # 0.01 short-paths - normalize_type_path
  Calling [Env.normalize_type_path] for prev/371
  # 0.01 short-paths - find_type_simple
  Short: t/281[1]
  # 0.01 short-paths - find_type_simple
  Initial: prev/283[3]
  # 0.01 short-paths - normalize_type_path
  Found type expansion t/281[1] for prev/283[3]
  # 0.01 short-paths - normalize_type_path
  Calling [Env.normalize_type_path] for t/281[1]
  # 0.01 short-paths - find_type_simple
  Canon: t/281[1]
  # 0.01 short-paths - normalize_type_path
  Found type expansion t/281[1] for prev/283[3]
  # 0.01 short-paths - normalize_type_path
  Calling [Env.normalize_type_path] for t/281[1]
  # 0.01 short-paths - normalize_type_path
  Calling [Env.normalize_type_path] for t/284[4]
  # 0.01 short-paths - fill_by_level
  Treating t/281[1] (t/284[4])
  # 0.01 short-paths - normalize_type_path
  Found type expansion t/281[1] for prev/283[3]
  # 0.01 short-paths - normalize_type_path
  Calling [Env.normalize_type_path] for t/281[1]
  # 0.01 short-paths - normalize_type_path
  Calling [Env.normalize_type_path] for X/289[2].t
  # 0.01 short-paths - fill_by_level
  Treating X/289[2].t (X/289[2].t)
  # 0.01 short-paths - fill_by_level
  Empty queue
  # 0.01 short-paths - normalize_type_path
  Found type expansion t/281[1] for prev/283[3]
  # 0.01 short-paths - normalize_type_path
  Calling [Env.normalize_type_path] for t/281[1]
  # 0.01 short-paths - find_type_simple
  Short: prev/283[3]
  # 0.01 short-paths - find_type_simple
  Initial: t/284[4]
  # 0.01 short-paths - normalize_type_path
  Calling [Env.normalize_type_path] for t/284[4]
  # 0.01 short-paths - find_type_simple
  Canon: t/284[4]
  # 0.01 short-paths - normalize_type_path
  Calling [Env.normalize_type_path] for t/284[4]
  # 0.01 short-paths - normalize_type_path
  Calling [Env.normalize_type_path] for t/284[4]
  # 0.01 short-paths - fill_by_level
  Treating t/281[1] (t/284[4])
  # 0.01 short-paths - fill_by_level
  Empty queue
  # 0.01 short-paths - normalize_type_path
  Calling [Env.normalize_type_path] for t/284[4]
  # 0.01 short-paths - normalize_type_path
  Calling [Env.normalize_type_path] for t/284[4]
  # 0.01 short-paths - find_type_simple
  Short: t/281[1]
  # 0.01 short-paths - find_type_simple
  Initial: t/281[1]
  # 0.01 short-paths - normalize_type_path
  Calling [Env.normalize_type_path] for t/281[1]
  # 0.01 short-paths - find_type_simple
  Canon: t/281[1]
  # 0.01 short-paths - normalize_type_path
  Calling [Env.normalize_type_path] for prev/373
  # 0.01 short-paths - find_type_simple
  Short: t/281[1]
  # 0.01 short-paths - find_type_simple
  Initial: prev/283[3]
  # 0.01 short-paths - normalize_type_path
  Found type expansion t/281[1] for prev/283[3]
  # 0.01 short-paths - normalize_type_path
  Calling [Env.normalize_type_path] for t/281[1]
  # 0.01 short-paths - find_type_simple
  Canon: t/281[1]
  # 0.01 short-paths - normalize_type_path
  Found type expansion t/281[1] for prev/283[3]
  # 0.01 short-paths - normalize_type_path
  Calling [Env.normalize_type_path] for t/281[1]
  # 0.01 short-paths - find_type_simple
  Short: prev/283[3]
  # 0.01 short-paths - find_type_simple
  Initial: t/284[4]
  # 0.01 short-paths - normalize_type_path
  Calling [Env.normalize_type_path] for t/284[4]
  # 0.01 short-paths - find_type_simple
  Canon: t/284[4]
  # 0.01 short-paths - normalize_type_path
  Calling [Env.normalize_type_path] for t/284[4]
  # 0.01 short-paths - normalize_type_path
  Calling [Env.normalize_type_path] for t/284[4]
  # 0.01 short-paths - find_type_simple
  Short: t/281[1]
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
        "type": "sig type prev = t type t = Bar val err : prev -> t/2 end",
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
        "type": "sig type prev = t type t = Bar val err : prev -> t/2 end",
        "tail": "no"
      }
    ],
    "notifications": []
  }
