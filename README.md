# ppx_deriving_variant_string

A PPX deriver that generates string conversion functions for variant and
polymorphic variant types.

Provides four derivers:

| Deriver | Style | Generated functions |
|---|---|---|
| `[@@deriving toString]` | Reason (camelCase) | `typeNameToString : t -> string` |
| `[@@deriving fromString]` | Reason (camelCase) | `typeNameFromString : string -> t option`, `typeNameFromStringExn : string -> t` |
| `[@@deriving to_string]` | OCaml (snake_case) | `type_name_to_string : t -> string` |
| `[@@deriving of_string]` | OCaml (snake_case) | `type_name_of_string : string -> t option`, `type_name_of_string_exn : string -> t` |

When the type name is `t`, the prefix is dropped: `toString`, `fromString`,
`fromStringExn`, `to_string`, `of_string`, and `of_string_exn`.

## Usage

### Basic variant

```ocaml
type color =
  | Red
  | Green
  | Blue
[@@deriving toString, fromString]

let () =
  assert (colorToString Red = "Red");
  assert (colorFromString "Green" = Some Green);
  assert (colorFromString "purple" = None);
  assert (colorFromStringExn "Blue" = Blue)
```

### Polymorphic variant

```ocaml
type direction = [ `North | `South | `East | `West ]
[@@deriving toString, fromString]

let () =
  assert (directionToString `North = "North");
  assert (directionFromString "South" = Some `South)
```

Polymorphic variant inheritance is supported when the inherited type has the
same deriver:

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
  assert (extended_of_string "baz" = Some `Baz)
```

### Overriding the string representation

Use `[@as "..."]` or `[@name "..."]` to control the string value used for a
constructor:

```ocaml
type status =
  | Active [@as "active"]
  | Inactive [@name "inactive"]
  | Pending
[@@deriving toString, fromString]

let () =
  assert (statusToString Active = "active");
  assert (statusToString Pending = "Pending");
  assert (statusFromString "inactive" = Some Inactive);
  assert (statusFromString "Active" = None)
```

Both attributes behave identically. If both are present, `[@as]` takes
precedence.

### OCaml style

```ocaml
type my_type =
  | Foo
  | Bar [@as "bar"]
[@@deriving to_string, of_string]

let () =
  assert (my_type_to_string Foo = "Foo");
  assert (my_type_of_string "bar" = Some Bar);
  assert (my_type_of_string_exn "bar" = Bar)
```

## Payload constructors

Payload constructors are rejected by default because the deriver cannot infer a
lossless string conversion for their arguments.

Use `[@no_args]` when only the constructor tag matters. The generated
`to_string`/`toString` function ignores the payload, while
`of_string`/`fromString` skips that constructor because it cannot reconstruct
the payload.

```ocaml
type event =
  | Started
  | Unknown of string [@no_args]
[@@deriving to_string, of_string]

let () =
  assert (event_to_string (Unknown "x") = "Unknown");
  assert (event_of_string "Unknown" = None)
```

Use `[@no_string_exn]` when stringifying the constructor should fail loudly:

```ocaml
type event =
  | Started
  | Unknown of string [@no_string_exn]
[@@deriving to_string, of_string]
```

`event_to_string (Unknown "x")` raises `Failure`, and `event_of_string` skips
`Unknown`.

For polymorphic variants only, `[@no_string]` also narrows the return type of
`of_string`/`fromString` so the marked constructor is excluded from parser
results:

```ocaml
type event =
  [ `Started
  | `Unknown of string [@no_string]
  ]
[@@deriving to_string, of_string]

let parsed : [ `Started ] option = event_of_string "Started"
```

## Limitations

The derivers reject:

- Records
- Abstract types without a polymorphic variant manifest
- Open types
- Payload constructors unless they use `[@no_args]`, `[@no_string_exn]`, or
  polymorphic-variant-only `[@no_string]`
- Non-type polymorphic variant inheritance

## Building

```sh
dune build
dune test
```
