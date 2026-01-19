open Ocaml_typing
open Discourse_types

let test_subst_1 =
  let open Alcotest in
  test_case "test subst 1" `Quick (fun () ->
      let w = Ident.create_local "W" in
      let path =
        Path.(Pdot (Pdot (Pdot (Pdot (Pident w, "A"), "B"), "C"), "t"))
      in
      let lid = Untypeast.lident_of_path path in
      let wm_path = Path.(Pdot (Pident w, "M")) in
      let wm_lid = Untypeast.lident_of_path wm_path in
      let paths =
        Discourse_types.singleton lid (Type, path)
        |> Discourse_types.add wm_lid (Module, wm_path)
      in
      let substs =
        let path_subst_wnc = Path.(Pdot (Pdot (Pident w, "N"), "C")) in
        let lid_subst_wn = Ocaml_parsing.Longident.(Ldot (Lident "W", "N")) in
        let path_subst_ab =
          let a = Ident.create_local "A" in
          Path.(Pdot (Pident a, "B"))
        in
        let lid_subst_n = Ocaml_parsing.Longident.Lident "N" in
        let path_subst_nc =
          let n = Ident.create_local "N" in
          Path.(Pdot (Pident n, "C"))
        in
        let lid_subst_m = Ocaml_parsing.Longident.Lident "M" in
        Path.Map.singleton path_subst_ab (Lid_set.singleton lid_subst_n)
        |> Path.Map.add path_subst_nc (Lid_set.singleton lid_subst_m)
        |> Path.Map.add path_subst_wnc (Lid_set.singleton lid_subst_wn)
      in
      let result = Shorter_paths.apply_substitutions_fixpoint paths substs in
      let expected =
        "W.N.t [W/3.A.B.C.t]; W.N.C.t [W/3.A.B.C.t]; W.M [W/3.M]; W.M.t \
         [W/3.A.B.C.t];\n\
         W.A.B.C.t [W/3.A.B.C.t]"
      in
      let computed =
        Lid_trie.pp_seq Format.str_formatter result;
        Format.flush_str_formatter ()
      in
      check string "should be equal" expected computed)

let () = Alcotest.run "merlin-lib.short-paths" [ ("subst", [ test_subst_1 ]) ]
