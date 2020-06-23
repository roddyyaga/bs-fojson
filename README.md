"fojson" = "faux (yo)json"

There are [bs-yojson](https://github.com/roddyyaga/bs-yojson) provides direct BuckleScript bindings for
[yojson](https://github.com/ocaml-community/yojson). However, they produce a large amount of JavaScript (because yojson
implements JSON parsing/printing itself). This package rewrites parts of Yojson to reduce the size of the JavaScript
produced by using JavaScript JSON parsing/printing. It should also be faster because of this. There are some changes to
the API as specified in `yojson.mli` (for instance a lot of undocumented functions are removed) but it should stay the
same for the purposes of `ppx_yojson_conv` and `ppx_deriving_yojson`.

Changes that have been made:
- Removed undocumented functions
- Changed yojson-to-string implementations to go via `Js.Json`
- Changed string-to-yojson implementations to go via `Js.Json` and a hack to account for the JSON representation of ints
  and floats being the same
- Changed default mode to standard JSON rather than OCaml extensions
