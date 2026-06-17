(* Heavily based on deriving.show tests:
   https://github.com/ocaml-ppx/ppx_deriving/blob/35dfd4ad83e58bcfbc03b564e5fe6df06b6cbdd7/src_test/show/test_deriving_show.cppo.ml
*)

open OUnit2

let printer x = x
let opt_printer f x = match x with Some v -> "Some " ^ f v | None -> "None"

type variant = First [@as "first"] | Second | Third [@name "3rd"]
[@@deriving of_string, to_string]

let test_variant ctxt =
  let printer = opt_printer variant_to_string in
  assert_equal ~printer (Some First) (variant_of_string "first");
  assert_equal ~printer (Some Second) (variant_of_string "Second");
  assert_equal ~printer (Some Third) (variant_of_string "3rd");
  assert_equal ~printer None (variant_of_string "First")

type polyvar = [ `first [@as "First"] | `second ]
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

let test_module_exn _ctxt =
  assert_equal N.A (N.of_string_exn "A");
  assert_raises
    (Invalid_argument "Dune__exe__Test_of_string.N.of_string_exn: B") (fun () ->
      N.of_string_exn "B")

type base = [ `Foo [@name "foo"] | `Bar [@name "bar"] ]
[@@deriving of_string, to_string]

type extended = [ base | `Baz [@name "baz"] ] [@@deriving of_string, to_string]

let test_inherit ctxt =
  let printer = opt_printer extended_to_string in
  assert_equal ~printer (Some `Foo) (extended_of_string "foo");
  assert_equal ~printer (Some `Bar) (extended_of_string "bar");
  assert_equal ~printer (Some `Baz) (extended_of_string "baz");
  assert_equal ~printer None (extended_of_string "unknown")

(* [@no_args] payload constructors are skipped by of_string. *)
type with_no_args = Known [@name "known"] | Tagged of int [@no_args]
[@@deriving of_string, to_string]

let test_no_args_classic ctxt =
  let printer = opt_printer with_no_args_to_string in
  assert_equal ~printer (Some Known) (with_no_args_of_string "known");
  assert_equal ~printer None (with_no_args_of_string "Tagged");
  assert_equal ~printer None (with_no_args_of_string "anything")

type poly_with_no_args =
  [ `Known [@name "known"] | `Unknown of string [@no_args] ]
[@@deriving of_string, to_string]

let test_no_args_poly ctxt =
  let printer = opt_printer poly_with_no_args_to_string in
  assert_equal ~printer (Some `Known) (poly_with_no_args_of_string "known");
  assert_equal ~printer None (poly_with_no_args_of_string "Unknown");
  assert_equal ~printer None (poly_with_no_args_of_string "anything")

(* [@no_string_exn]: of_string skips the constructor; the return type is
   still the full type so callers can hold a value of the marked
   constructor built elsewhere. *)
type with_no_string_exn =
  | KnownC2 [@name "k"]
  | UnknownC2 of string [@no_string_exn]
[@@deriving of_string, to_string]

let test_no_string_exn_classic ctxt =
  let printer = opt_printer (fun _ -> "<value>") in
  assert_equal ~printer (Some KnownC2) (with_no_string_exn_of_string "k");
  assert_equal ~printer None (with_no_string_exn_of_string "UnknownC2");
  assert_equal ~printer None (with_no_string_exn_of_string "anything")

type poly_with_no_string_exn =
  [ `KnownE [@name "kp"] | `UnknownE of string [@no_string_exn] ]
[@@deriving of_string, to_string]

let test_no_string_exn_poly ctxt =
  let printer = opt_printer (fun _ -> "<value>") in
  assert_equal ~printer (Some `KnownE) (poly_with_no_string_exn_of_string "kp");
  assert_equal ~printer None (poly_with_no_string_exn_of_string "UnknownE");
  assert_equal ~printer None (poly_with_no_string_exn_of_string "anything")

(* [@no_string] (poly-only): of_string both skips the case AND narrows the
   return type. Constructing [`Unknown _] from the result is a type error.
   The annotation below documents the narrowed shape - the compiler enforces
   it via the inline polyvariant signature emitted by the deriver. *)
type poly_with_no_string =
  [ `KnownN [@name "k"] | `UnknownN of string [@no_string] ]
[@@deriving of_string, to_string]

let test_no_string_poly ctxt =
  let printer = opt_printer (fun _ -> "<value>") in
  (* Result type is narrowed to [`KnownN] (excludes [`UnknownN _]). *)
  let v : [ `KnownN ] option = poly_with_no_string_of_string "k" in
  assert_equal ~printer (Some `KnownN) v;
  assert_equal ~printer None (poly_with_no_string_of_string "UnknownN");
  assert_equal ~printer None (poly_with_no_string_of_string "anything")

let suite =
  "Test deriving(of_string)"
  >::: [
         "test_variant" >:: test_variant;
         "test_polyvar" >:: test_polyvar;
         "test_module" >:: test_module;
         "test_variant_exn" >:: test_variant_exn;
         "test_polyvar_exn" >:: test_polyvar_exn;
         "test_module_exn" >:: test_module_exn;
         "test_inherit" >:: test_inherit;
         "test_no_args_classic" >:: test_no_args_classic;
         "test_no_args_poly" >:: test_no_args_poly;
         "test_no_string_exn_classic" >:: test_no_string_exn_classic;
         "test_no_string_exn_poly" >:: test_no_string_exn_poly;
         "test_no_string_poly" >:: test_no_string_poly;
       ]

let () = run_test_tt_main suite
