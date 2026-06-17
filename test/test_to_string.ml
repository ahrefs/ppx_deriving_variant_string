(* Heavily based on deriving.show tests:
   https://github.com/ocaml-ppx/ppx_deriving/blob/35dfd4ad83e58bcfbc03b564e5fe6df06b6cbdd7/src_test/show/test_deriving_show.cppo.ml
*)

open OUnit2

let printer x = x

type v = Foo | Bar | Baz [@@deriving to_string]

let test_variant ctxt =
  assert_equal ~printer "Foo" (v_to_string Foo);
  assert_equal ~printer "Bar" (v_to_string Bar);
  assert_equal ~printer "Baz" (v_to_string Baz)

type pv1 = [ `Foo | `Bar [@as "bar"] ] [@@deriving to_string]

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

type foo = F | B | C
and bar = A | D [@@deriving to_string]

let test_mrec ctxt =
  let e1 = B in
  assert_equal ~printer "B" (foo_to_string e1)

type variant_string =
  | First [@as "first"]
  | Second [@name "second"]
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

(* [@no_args]: constructor carries a payload but to_string drops it
   and emits just the (possibly-renamed) constructor tag. *)
type with_no_args =
  | Plain
  | Tagged of int [@no_args]
  | Renamed of string [@name "renamed"] [@no_args]
[@@deriving to_string]

let test_no_args_classic ctxt =
  assert_equal ~printer "Plain" (with_no_args_to_string Plain);
  assert_equal ~printer "Tagged" (with_no_args_to_string (Tagged 42));
  assert_equal ~printer "renamed" (with_no_args_to_string (Renamed "ignored"))

type poly_with_no_args =
  [ `Known [@name "known"]
  | `Unknown of string [@no_args]
  | `Wrapped of int [@name "wrapped"] [@no_args] ]
[@@deriving to_string]

let test_no_args_poly ctxt =
  assert_equal ~printer "known" (poly_with_no_args_to_string `Known);
  assert_equal ~printer "Unknown"
    (poly_with_no_args_to_string (`Unknown "anything"));
  assert_equal ~printer "wrapped" (poly_with_no_args_to_string (`Wrapped 99))

(* [@no_string_exn]: to_string raises on the marked constructor.
   Available on both classic and polymorphic variants. of_string skips
   the constructor (its return type is still the full type). *)
type with_no_string_exn = KnownC | UnknownC of string [@no_string_exn]
[@@deriving to_string]

let test_no_string_exn_classic ctxt =
  assert_equal ~printer "KnownC" (with_no_string_exn_to_string KnownC);
  assert_raises
    (Failure
       "to_string: cannot stringify constructor UnknownC (marked \
        [@no_string_exn])") (fun () ->
      with_no_string_exn_to_string (UnknownC "anything"))

type poly_with_no_string_exn =
  [ `KnownE [@name "known"] | `UnknownE of string [@no_string_exn] ]
[@@deriving to_string]

let test_no_string_exn_poly ctxt =
  assert_equal ~printer "known" (poly_with_no_string_exn_to_string `KnownE);
  assert_raises
    (Failure
       "to_string: cannot stringify constructor `UnknownE (marked \
        [@no_string_exn])") (fun () ->
      poly_with_no_string_exn_to_string (`UnknownE "anything"))

(* [@no_string] (poly-only): to_string raises (same as [@no_string_exn]);
   of_string also narrows the return type so [`Unknown _] is not
   constructible by the parser. *)
type poly_with_no_string =
  [ `KnownN [@name "known"] | `UnknownN of string [@no_string] ]
[@@deriving to_string]

let test_no_string_poly ctxt =
  assert_equal ~printer "known" (poly_with_no_string_to_string `KnownN);
  assert_raises
    (Failure
       "to_string: cannot stringify constructor `UnknownN (marked [@no_string])")
    (fun () -> poly_with_no_string_to_string (`UnknownN "anything"))

let suite =
  "Test deriving(to_string)"
  >::: [
         "test_variant" >:: test_variant;
         "test_poly" >:: test_poly;
         "test_mrec" >:: test_mrec;
         "test_variant_printer" >:: test_variant_printer;
         "test_module" >:: test_module;
         "test_no_args_classic" >:: test_no_args_classic;
         "test_no_args_poly" >:: test_no_args_poly;
         "test_no_string_exn_classic" >:: test_no_string_exn_classic;
         "test_no_string_exn_poly" >:: test_no_string_exn_poly;
         "test_no_string_poly" >:: test_no_string_poly;
       ]

let () = run_test_tt_main suite
