# ppx_deriving_variant_string

OCaml PPX deriver that generates converters between regular or polymorphic
variants and strings. Supports both OCaml and Reason casing.

## Quick Start

Install: `opam install ppx_deriving_variant_string`.

In Reason syntax:

```reason
[@deriving (fromString, toString)]
type foo =
  | [@as "first"] First
  | Second;

let a = fooFromString("first"); /* Some(First) */
let b = fooFromString("Second"); /* Some(Second) */
let c = fooFromString("First"); /* None */
let d = fooToString(First); /* "first" */
let e = fooToString(Second); /* "Second" */
```

In OCaml syntax:

```ocaml
type foo =
  | First [@as "first"]
  | Second
[@@deriving of_string, to_string]

let a = foo_of_string "first" (* Some(First) *)
let b = foo_of_string "Second" (* Some(Second) *)
let c = foo_of_string "First" (* None *)
let d = foo_to_string First (* "first" *)
let e = foo_to_string Second (* "Second" *)
```

## Name mangling

If the type where the PPX is applied is named `t`, the generated functions won't
include any prefix and will be just `toString`, `fromString` (or `to_string`,
`of_string`).

## Why not ppx_deriving `show`?

The original `ppx_deriving` includes a plugin named
[show](https://github.com/ocaml-ppx/ppx_deriving#plugin-show) which has some
overlap in functionality. However it was missing a few things:

- it doesn't include functionality to convert from strings to variants
- `show`shows a backtick for polymorphic variants
- it does support `as` (it's called `printer`), which supports cases with
  variants with payloads, but it's a bit more heavyweight, as one has to pass a
  formatter instead of just a
  string, [example](https://github.com/ocaml-ppx/ppx_deriving/blob/35dfd4ad83e58bcfbc03b564e5fe6df06b6cbdd7/src_test/show/test_deriving_show.cppo.ml#L219)
- `printer` is only supported in polyvars for some reason, but not on regular
  variants, which was a feature we wanted to have

## Why not Melange `jsConverter`?

We originally used Melange
[jsConverter](https://melange.re/v2.0.0/communicate-with-javascript/#conversion-functions),
but we ran into limitations when making code compatible with [universal
libraries](https://melange.re/blog/posts/dune-universal-libraries-preview), that
have to run both on the client and the server.
