(* Heavily based on deriving.show tests:
   https://github.com/ocaml-ppx/ppx_deriving/blob/35dfd4ad83e58bcfbc03b564e5fe6df06b6cbdd7/src_test/show/test_deriving_show.cppo.ml
*)

open OUnit2

let printer x = x

type v =
  | Foo
  | Bar
  | Baz
[@@deriving toString]
let test_variant ctxt =
  assert_equal ~printer "Foo" (vToString Foo);
  assert_equal ~printer "Bar" (vToString Bar);
  assert_equal ~printer "Baz" (vToString Baz)

type pv1 =
  [ `Foo
  | `Bar [@as "bar"]
  ]
[@@deriving toString]
let test_poly ctxt =
  assert_equal ~printer "Foo" (pv1ToString `Foo);
  assert_equal ~printer "bar" (pv1ToString `Bar)

(* type pv2 =
     [ `Baz
     | pv1
     ]
   [@@deriving toString]
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
[@@deriving toString]

let test_mrec ctxt =
  let e1 = B in
  assert_equal ~printer "B" (fooToString e1)

type variant_string =
  | First [@as "first"]
  | Second [@as "second"]
  | Third
  | Fourth [@as "4th"]
[@@deriving toString]

let test_variant_printer ctxt =
  assert_equal ~printer "first" (variant_stringToString First);
  assert_equal ~printer "second" (variant_stringToString Second);
  assert_equal ~printer "Third" (variant_stringToString Third);
  assert_equal ~printer "4th" (variant_stringToString Fourth)

module M : sig
  type t = A [@@deriving toString]
end = struct
  type t = A [@@deriving toString]
end

let test_module ctxt = assert_equal ~printer "A" (M.toString M.A)

let suite =
  "Test deriving(toString)"
  >::: [
         "test_variant" >:: test_variant;
         "test_poly" >:: test_poly;
         "test_mrec" >:: test_mrec;
         "test_variant_printer" >:: test_variant_printer;
         "test_module" >:: test_module;
       ]

let _ = run_test_tt_main suite
