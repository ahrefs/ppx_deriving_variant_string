(* Heavily based on deriving.show tests:
   https://github.com/ocaml-ppx/ppx_deriving/blob/35dfd4ad83e58bcfbc03b564e5fe6df06b6cbdd7/src_test/show/test_deriving_show.cppo.ml
*)

open OUnit2

let printer x = x

type v =
  | Foo
  | Bar
  | Baz
[@@deriving to_string]
let test_variant ctxt =
  assert_equal ~printer "Foo" (v_to_string Foo);
  assert_equal ~printer "Bar" (v_to_string Bar);
  assert_equal ~printer "Baz" (v_to_string Baz)

type pv1 =
  [ `Foo
  | `Bar [@as "bar"]
  ]
[@@deriving to_string]
let test_poly ctxt =
  assert_equal ~printer "Foo" (pv1_to_string `Foo);
  assert_equal ~printer "bar" (pv1_to_string `Bar)

(* type pv2 =
     [ `Baz
     | pv1
     ]
   [@@deriving to_string]
   let test_poly_inherit ctxt =
     assert_equal ~printer "`Foo" (show_pv2 `Foo);
     assert_equal ~printer "`Baz" (show_pv2 `Baz) *)

type foo =
  | F
  | B
  | C
and bar =
  | A
  | D
[@@deriving to_string]

let test_mrec ctxt =
  let e1 = B in
  assert_equal ~printer "B" (foo_to_string e1)

type variant_string =
  | First [@as "first"]
  | Second [@as "second"]
  | Third
  | Fourth [@as "4th"]
[@@deriving to_string]

let test_variant_printer ctxt =
  assert_equal ~printer "first" (variant_string_to_string First);
  assert_equal ~printer "second" (variant_string_to_string Second);
  assert_equal ~printer "Third" (variant_string_to_string Third);
  assert_equal ~printer "4th" (variant_string_to_string Fourth)

module M : sig
  type t = A [@@deriving to_string]
end = struct
  type t = A [@@deriving to_string]
end

let test_module ctxt = assert_equal ~printer "A" (M.to_string M.A)

let suite =
  "Test deriving(to_string)"
  >::: [
         "test_variant" >:: test_variant;
         "test_poly" >:: test_poly;
         "test_mrec" >:: test_mrec;
         "test_variant_printer" >:: test_variant_printer;
         "test_module" >:: test_module;
       ]

let _ = run_test_tt_main suite
