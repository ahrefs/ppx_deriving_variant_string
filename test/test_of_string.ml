(* Heavily based on deriving.show tests:
   https://github.com/ocaml-ppx/ppx_deriving/blob/35dfd4ad83e58bcfbc03b564e5fe6df06b6cbdd7/src_test/show/test_deriving_show.cppo.ml
*)

open OUnit2

let printer x = x
let opt_printer f x = match x with Some v -> "Some " ^ f v | None -> "None"

type variant = First [@as "first"] | Second | Third [@name "3rd"]
[@@deriving of_string, to_string]

let test_variant _ctxt =
  let printer = opt_printer variant_to_string in
  assert_equal ~printer (Some First) (variant_of_string "first");
  assert_equal ~printer (Some Second) (variant_of_string "Second");
  assert_equal ~printer (Some Third) (variant_of_string "3rd");
  assert_equal ~printer None (variant_of_string "First")

type polyvar = [ `first [@as "First"] | `second ]
[@@deriving of_string, to_string]

let test_polyvar _ctxt =
  let printer = opt_printer polyvar_to_string in
  assert_equal ~printer (Some `first) (polyvar_of_string "First");
  assert_equal ~printer (Some `second) (polyvar_of_string "second");
  assert_equal ~printer None (polyvar_of_string "first")

module N : sig
  type t = A [@@deriving of_string, to_string]
end = struct
  type t = A [@@deriving of_string, to_string]
end

let test_module _ctxt =
  let printer = opt_printer N.to_string in
  assert_equal ~printer (Some N.A) (N.of_string "A");
  assert_equal N.A (N.of_string_exn "A");
  assert_raises
    (Invalid_argument "Dune__exe__Test_of_string.N.of_string_exn: B") (fun () ->
      N.of_string_exn "B")

let test_variant_exn _ctxt =
  assert_equal First (variant_of_string_exn "first");
  assert_equal Second (variant_of_string_exn "Second");
  assert_equal Third (variant_of_string_exn "3rd");
  assert_raises
    (Invalid_argument "Dune__exe__Test_of_string.variant_of_string_exn: First")
    (fun () -> variant_of_string_exn "First")

let test_polyvar_exn _ctxt =
  assert_equal `first (polyvar_of_string_exn "First");
  assert_equal `second (polyvar_of_string_exn "second");
  assert_raises
    (Invalid_argument "Dune__exe__Test_of_string.polyvar_of_string_exn: first")
    (fun () -> polyvar_of_string_exn "first")

type base = [ `Foo [@name "foo"] | `Bar [@name "bar"] ]
[@@deriving of_string, to_string]

type extended = [ base | `Baz [@name "baz"] ] [@@deriving of_string, to_string]

let test_inherit _ctxt =
  let printer = opt_printer extended_to_string in
  assert_equal ~printer (Some `Foo) (extended_of_string "foo");
  assert_equal ~printer (Some `Bar) (extended_of_string "bar");
  assert_equal ~printer (Some `Baz) (extended_of_string "baz");
  assert_equal ~printer None (extended_of_string "unknown");
  assert_equal `Foo (extended_of_string_exn "foo");
  assert_equal `Bar (extended_of_string_exn "bar");
  assert_equal `Baz (extended_of_string_exn "baz")

let suite =
  "Test deriving(of_string)"
  >::: [
         "test_variant" >:: test_variant;
         "test_polyvar" >:: test_polyvar;
         "test_module" >:: test_module;
         "test_variant_exn" >:: test_variant_exn;
         "test_polyvar_exn" >:: test_polyvar_exn;
         "test_inherit" >:: test_inherit;
       ]

let () = run_test_tt_main suite
