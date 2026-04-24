(* Heavily based on deriving.show tests:
   https://github.com/ocaml-ppx/ppx_deriving/blob/35dfd4ad83e58bcfbc03b564e5fe6df06b6cbdd7/src_test/show/test_deriving_show.cppo.ml
*)

open OUnit2

let printer x = x
let opt_printer f x = match x with Some v -> "Some " ^ f v | None -> "None"

type variant = First [@as "first"] | Second | Third [@name "3rd"]
[@@deriving fromString, toString]

let test_variant _ctxt =
  let printer = opt_printer variantToString in
  assert_equal ~printer (Some First) (variantFromString "first");
  assert_equal ~printer (Some Second) (variantFromString "Second");
  assert_equal ~printer (Some Third) (variantFromString "3rd");
  assert_equal ~printer None (variantFromString "First")

type polyvar = [ `first [@as "First"] | `second ]
[@@deriving fromString, toString]

let test_polyvar _ctxt =
  let printer = opt_printer polyvarToString in
  assert_equal ~printer (Some `first) (polyvarFromString "First");
  assert_equal ~printer (Some `second) (polyvarFromString "second");
  assert_equal ~printer None (polyvarFromString "first")

module N : sig
  type t = A [@@deriving fromString, toString]
end = struct
  type t = A [@@deriving fromString, toString]
end

let test_module _ctxt =
  let printer = opt_printer N.toString in
  assert_equal ~printer (Some N.A) (N.fromString "A");
  assert_equal N.A (N.fromStringExn "A");
  assert_raises
    (Invalid_argument "Dune__exe__Test_fromString.N.fromStringExn: B")
    (fun () -> N.fromStringExn "B")

let test_variant_exn _ctxt =
  assert_equal First (variantFromStringExn "first");
  assert_equal Second (variantFromStringExn "Second");
  assert_equal Third (variantFromStringExn "3rd");
  assert_raises
    (Invalid_argument "Dune__exe__Test_fromString.variantFromStringExn: First")
    (fun () -> variantFromStringExn "First")

let test_polyvar_exn _ctxt =
  assert_equal `first (polyvarFromStringExn "First");
  assert_equal `second (polyvarFromStringExn "second");
  assert_raises
    (Invalid_argument "Dune__exe__Test_fromString.polyvarFromStringExn: first")
    (fun () -> polyvarFromStringExn "first")

type base = [ `Foo [@name "foo"] | `Bar [@name "bar"] ]
[@@deriving fromString, toString]

type extended = [ base | `Baz [@name "baz"] ] [@@deriving fromString, toString]

let test_inherit _ctxt =
  let printer = opt_printer extendedToString in
  assert_equal ~printer (Some `Foo) (extendedFromString "foo");
  assert_equal ~printer (Some `Bar) (extendedFromString "bar");
  assert_equal ~printer (Some `Baz) (extendedFromString "baz");
  assert_equal ~printer None (extendedFromString "unknown");
  assert_equal `Foo (extendedFromStringExn "foo");
  assert_equal `Bar (extendedFromStringExn "bar");
  assert_equal `Baz (extendedFromStringExn "baz")

let suite =
  "Test deriving(fromString)"
  >::: [
         "test_variant" >:: test_variant;
         "test_polyvar" >:: test_polyvar;
         "test_module" >:: test_module;
         "test_variant_exn" >:: test_variant_exn;
         "test_polyvar_exn" >:: test_polyvar_exn;
         "test_inherit" >:: test_inherit;
       ]

let () = run_test_tt_main suite
