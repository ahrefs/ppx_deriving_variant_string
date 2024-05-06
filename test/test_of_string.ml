(* Heavily based on deriving.show tests:
   https://github.com/ocaml-ppx/ppx_deriving/blob/35dfd4ad83e58bcfbc03b564e5fe6df06b6cbdd7/src_test/show/test_deriving_show.cppo.ml
*)

open OUnit2

let printer x = x
let opt_printer f x =
  match x with
  | Some v -> "Some " ^ f v
  | None -> "None"

type variant =
  | First [@as "first"]
  | Second
[@@deriving of_string, to_string]

let test_variant ctxt =
  let printer = opt_printer variant_to_string in
  assert_equal ~printer (Some First) (variant_of_string "first");
  assert_equal ~printer (Some Second) (variant_of_string "Second");
  assert_equal ~printer None (variant_of_string "First")

type polyvar =
  [ `first [@as "First"]
  | `second
  ]
[@@deriving of_string, to_string]

let test_polyvar ctxt =
  let printer = opt_printer polyvar_to_string in
  assert_equal ~printer (Some `first) (polyvar_of_string "First");
  assert_equal ~printer (Some `second) (polyvar_of_string "second");
  assert_equal ~printer None (polyvar_of_string "first")

module N : sig
  type t = A [@@deriving of_string, to_string]
end = struct
  type t = A [@@deriving of_string, to_string]
end

let test_module ctxt =
  let printer = opt_printer N.to_string in
  assert_equal ~printer (Some N.A) (N.of_string "A")

let suite =
  "Test deriving(of_string)"
  >::: [ "test_variant" >:: test_variant; "test_polyvar" >:: test_polyvar; "test_module" >:: test_module ]

let _ = run_test_tt_main suite
