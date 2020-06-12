"fojson" = "faux (yo)json"

There are [bs-yojson](https://github.com/roddyyaga/bs-yojson) provides direct BuckleScript bindings for
[yojson](https://github.com/ocaml-community/yojson). However, they produce a large amount of JavaScript (because yojson
implements JSON parsing/printing). This package rewrites parts of Yojson to reduce the size of the JavaScript produced.
It should also be faster. There are some changes to the API as specified in `yojson.mli` (for instance a lot of
undocumented functions are removed) but it should stay the same for the purposes of `ppx_yojson_conv` and
`ppx_deriving_yojson`.

Changes that have been made:
- Removed undocumented functions
- Changed yojson-to-string implementations to go via `Js.Json`

Planned changes:
- Replace string-to-yojson parsing code with an implementation that uses `Js.Json`. The difficulty here is that Yojson
  needs to be able to parse floats with integer values (e.g. `1.0`) and integers differently but JavaScript can't do
  that (since it only has a float type). So my hacky plan to get around this is to wrap numbers in strings before
  parsing, then convert them correctly afterwards.
