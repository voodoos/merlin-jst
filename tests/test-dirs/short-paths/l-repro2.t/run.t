
Priv
  $ $OCAMLC -c -short-paths -no-alias-deps -o priv/repro2_priv__.cmo -c -impl priv/repro2_priv__.ml-gen 2>/dev/null
  $ $OCAMLC -c -short-paths -no-alias-deps -I priv -open Repro2_priv__ -o priv/repro2_priv.cmo -c -impl priv/repro2_priv.ml
  $ $OCAMLC -c -short-paths -no-alias-deps -I priv -open Repro2_priv__ -o priv/repro2_priv__Topic.cmi -c -intf priv/topic.mli 
  $ $OCAMLC -c -short-paths -no-alias-deps -I priv -open Repro2_priv__ -o priv/repro2_priv__Topic_name.cmi -c -intf priv/topic_name.mli 

  $ ls priv/*.cmi
  priv/repro2_priv.cmi
  priv/repro2_priv__.cmi
  priv/repro2_priv__Topic.cmi
  priv/repro2_priv__Topic_name.cmi

Types
  $ $OCAMLC -c -short-paths -no-alias-deps -o types/repro2_types__.cmo -c -impl types/repro2_types__.ml-gen 2>/dev/null
  $ $OCAMLC -c -short-paths -no-alias-deps -I priv -I types -open Repro2_types__ -o types/repro2_types.cmo -c -impl types/repro2_types.ml
  $ $OCAMLC -c -short-paths -no-alias-deps -I priv -I types -open Repro2_types__ -o types/repro2_types__Topic.cmi -c -intf types/topic.mli 
  $ $OCAMLC -c -short-paths -no-alias-deps -I priv -I types -open Repro2_types__ -o types/repro2_types__Topic_name.cmi -c -intf types/topic_name.mli 

  $ ls types/*.cmi
  types/repro2_types.cmi
  types/repro2_types__.cmi
  types/repro2_types__Topic.cmi
  types/repro2_types__Topic_name.cmi

Main
  $ $OCAMLC -c -short-paths -no-alias-deps -I priv -I types -o main/repro2_main.cmo -c -impl main/repro2_main.ml

  $ ls main/*.cmi
  main/repro2_main.cmi

Usage
  $ $OCAMLC -c -short-paths -no-alias-deps -o usage/repro2_standalone__.cmo -c -impl usage/repro2_standalone__.ml-gen 2>/dev/null
  $ $OCAMLC -c -short-paths -no-alias-deps -I priv -I types -I main -I usage -open Repro2_standalone__ -o usage/repro2_standalone.cmo -c -impl usage/repro2_standalone.ml
  $ $OCAMLC -c -short-paths -no-alias-deps -I priv -I types -I main -I usage -open Repro2_standalone__ -o usage/repro2_standalone__Usage.cmo -c -impl usage/usage.ml

  $ ls usage/*.cmi
  usage/repro2_standalone.cmi
  usage/repro2_standalone__.cmi
  usage/repro2_standalone__Usage.cmi


  $ cat >.merlin <<'EOF'
  > FLG -short-paths -nostdlib
  > B .
  > B priv
  > B types
  > B main
  > B usage
  > EOF

  $ $MERLIN single type-enclosing -log-file log -position 1:40 -index 0 -open Repro2_standalone__ \
  > -filename usage/usage.ml <usage/usage.ml | jq '.value[0].type'
  "Repro2_priv.Topic.t -> Repro2_main.Topic_name.t option"
