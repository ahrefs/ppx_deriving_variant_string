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
[@@deriving fromString, toString]

let test_variant ctxt =
  let printer = opt_printer variantToString in
  assert_equal ~printer (Some First) (variantFromString "first");
  assert_equal ~printer (Some Second) (variantFromString "Second");
  assert_equal ~printer None (variantFromString "First")

type polyvar =
  [ `first [@as "First"]
  | `second
  ]
[@@deriving fromString, toString]

let test_polyvar ctxt =
  let printer = opt_printer polyvarToString in
  assert_equal ~printer (Some `first) (polyvarFromString "First");
  assert_equal ~printer (Some `second) (polyvarFromString "second");
  assert_equal ~printer None (polyvarFromString "first")

module N : sig
  type t = A [@@deriving fromString, toString]
end = struct
  type t = A [@@deriving fromString, toString]
end

let test_module ctxt =
  let printer = opt_printer N.toString in
  assert_equal ~printer (Some N.A) (N.fromString "A")

let suite =
  "Test deriving(fromString)"
  >::: [ "test_variant" >:: test_variant; "test_polyvar" >:: test_polyvar; "test_module" >:: test_module ]

let _ = run_test_tt_main suite
