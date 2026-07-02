# 1.1.0

- Import upstream deriver improvements:
  - add `[@name]` as an alias for `[@as]`
  - generate `fromStringExn` and `of_string_exn`
  - support polymorphic variant inheritance
  - add payload-constructor opt-ins with `[@no_args]`, `[@no_string_exn]`, and
    polymorphic-variant-only `[@no_string]`

# 1.0.1

- Fix public name, #2 by @anmonteiro

# 1.0.0

- Initial release, support for `toString`, `fromString`, `to_string` and
  `of_string` derivers
