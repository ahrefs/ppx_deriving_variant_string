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
  | [@name "second"] Second;

let a = fooFromString("first"); /* Some(First) */
let b = fooFromStringExn("second"); /* Second */
let c = fooToString(First); /* "first" */
let d = fooToString(Second); /* "second" */
```

In OCaml syntax:

```ocaml
type foo =
  | First [@as "first"]
  | Second [@name "second"]
[@@deriving of_string, to_string]

let a = foo_of_string "first"       (* Some First *)
let b = foo_of_string_exn "second"  (* Second *)
let c = foo_to_string First          (* "first" *)
let d = foo_to_string Second         (* "second" *)
```

## Generated functions

| Deriver | Style | Generated functions |
| --- | --- | --- |
| `[@@deriving toString]` | Reason (camelCase) | `typeNameToString : t -> string` |
| `[@@deriving fromString]` | Reason (camelCase) | `typeNameFromString : string -> t option`, `typeNameFromStringExn : string -> t` |
| `[@@deriving to_string]` | OCaml (snake_case) | `type_name_to_string : t -> string` |
| `[@@deriving of_string]` | OCaml (snake_case) | `type_name_of_string : string -> t option`, `type_name_of_string_exn : string -> t` |

If the type where the PPX is applied is named `t`, the generated functions won't
include any prefix and will be just `toString`, `fromString`, `fromStringExn`
(or `to_string`, `of_string`, `of_string_exn`).

## Overriding the string representation

Use `[@as "..."]` or `[@name "..."]` to control the string value used for a
constructor:

```ocaml
type status =
  | Active [@as "active"]
  | Inactive [@name "inactive"]
  | Pending
[@@deriving to_string, of_string]

let () =
  assert (status_to_string Active = "active");
  assert (status_to_string Inactive = "inactive");
  assert (status_of_string "inactive" = Some Inactive)
```

Both `[@as]` and `[@name]` behave identically; `[@as]` takes precedence if both
are present.

## Polymorphic variant inheritance

Inherited polymorphic variants are supported:

```ocaml
type base =
  [ `Foo [@name "foo"]
  | `Bar [@name "bar"]
  ]
[@@deriving to_string, of_string]

type extended =
  [ base
  | `Baz [@name "baz"]
  ]
[@@deriving to_string, of_string]

let () =
  assert (extended_to_string `Foo = "foo");
  assert (extended_of_string "bar" = Some `Bar);
  assert (extended_of_string_exn "baz" = `Baz)
```

## Limitations

The derivers only support nullary constructors (no payloads). The following will
raise a compile-time error:

- Variant constructors with arguments (`| Foo of int`)
- Record types (`type t = { ... }`)
- Abstract types
- Open types

## Why not ppx_deriving `show`?

The original `ppx_deriving` includes a plugin named
[show](https://github.com/ocaml-ppx/ppx_deriving#plugin-show) which has some
overlap in functionality. However it was missing a few things:

- it doesn't include functionality to convert from strings to variants
- `show` shows a backtick for polymorphic variants
- it does support `as` (it's called `printer`), which supports cases with
  variants with payloads, but it's a bit more heavyweight, as one has to pass a
  formatter instead of just a string,
  [example](https://github.com/ocaml-ppx/ppx_deriving/blob/35dfd4ad83e58bcfbc03b564e5fe6df06b6cbdd7/src_test/show/test_deriving_show.cppo.ml#L219)
- `printer` is only supported in polyvars for some reason, but not on regular
  variants, which was a feature we wanted to have

## Why not Melange `jsConverter`?

We originally used Melange
[jsConverter](https://melange.re/v2.0.0/communicate-with-javascript/#conversion-functions),
but we ran into limitations when making code compatible with [universal
libraries](https://melange.re/blog/posts/dune-universal-libraries-preview), that
have to run both on the client and the server.

## Building

```sh
dune build
dune test
```
