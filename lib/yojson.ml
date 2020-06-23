let version = "%%VERSION%%"

exception Json_error of string

let json_error s = raise (Json_error s)

exception End_of_input

let escape_at_symbol s = Js.String.replace "@" "!@" s

let unescape_exclamation_at s = Js.String.replace "!@" "@" s

let wrap_ints s =
  let callback matched captured _offset _whole_string =
    if Js.Undefined.testAny captured then matched else "\"@" ^ captured ^ "\""
  in
  (* See second answer here
   * https://stackoverflow.com/questions/6462578/regex-to-match-all-instances-not-inside-quotes *)
  Js.String.unsafeReplaceBy1 [%re {re|/\\"|"(?:\\"|[^"])*"|(\d+)/g|re}] callback
    s

let is_object_or_array x =
  match x with `List _ | `Assoc _ -> true | _ -> false

(** {3 Type of the JSON tree} *)

type t =
  [ `Null
  | `Bool of bool
  | `Int of int
  | `Intlit of string
  | `Float of float
  | `Floatlit of string
  | `String of string
  | `Stringlit of string
  | `Assoc of (string * t) list
  | `List of t list
  | `Tuple of t list
  | `Variant of string * t option ]
(**
All possible cases defined in Yojson:
- `Null: JSON null
- `Bool of bool: JSON boolean
- `Int of int: JSON number without decimal point or exponent.
- `Intlit of string: JSON number without decimal point or exponent,
	    preserved as a string.
- `Float of float: JSON number, Infinity, -Infinity or NaN.
- `Floatlit of string: JSON number, Infinity, -Infinity or NaN,
	    preserved as a string.
- `String of string: JSON string. Bytes in the range 128-255 are preserved
	    as-is without encoding validation for both reading
	    and writing.
- `Stringlit of string: JSON string literal including the double quotes.
- `Assoc of (string * json) list: JSON object.
- `List of json list: JSON array.
- `Tuple of json list: Tuple (non-standard extension of JSON).
	    Syntax: [("abc", 123)].
- `Variant of (string * json option): Variant (non-standard extension of JSON).
	    Syntax: [<"Foo">] or [<"Bar":123>].
*)

let rec from_processed_js js =
  let open Js.Json in
  match classify js with
  | JSONNull -> `Null
  | JSONTrue -> `Bool true
  | JSONFalse -> `Bool false
  | JSONNumber x -> `Float x
  | JSONString s ->
      if Js.String.get s 0 = "@" then
        `Int (s |> Js.String.sliceToEnd ~from:1 |> int_of_string)
      else `String s
  | JSONObject dict ->
      let pairs =
        Js.Dict.entries dict |> Array.to_list
        |> List.map (fun (k, v) -> (k, from_processed_js v))
      in
      `Assoc pairs
  | JSONArray xs -> `List (xs |> Array.to_list |> List.map from_processed_js)

type json = t
(**
 * Compatibility type alias for type `t`
 *)

(*
  Note to adventurers: ocamldoc does not support inline comments
  on each polymorphic variant, and cppo doesn't allow to concatenate
  comments, so it would be complicated to document only the
  cases that are preserved by cppo in the type definition.
*)

external js_json_int : int -> Js.Json.t = "%identity"

external js_json_float : float -> Js.Json.t = "%identity"

let rec to_js (x : t) =
  match x with
  | `Null -> Js.Json.null
  | `Bool b -> Js.Json.boolean b
  | `Int i -> js_json_int i
  | `Intlit i_string -> js_json_int (int_of_string i_string)
  | `Floatlit f_string -> js_json_float (float_of_string f_string)
  | `Float f -> Js.Json.number f
  | `String s -> Js.Json.string s
  | `Assoc pairs ->
      Js.Dict.fromList
        ((List.map (fun ((k : string), v) -> (k, to_js v))) pairs)
      |> Js.Json.object_
  | `List xs -> Js.Json.array (List.map to_js xs |> Array.of_list)
  | `Tuple xs -> Js.Json.array (List.map to_js xs |> Array.of_list)
  | `Variant (k, v_opt) -> (
      match v_opt with
      | Some v -> to_js (`Tuple [ `String k; v ])
      | None -> to_js (`String k) )
  | `Stringlit s ->
      (* Assume valid, i.e. starts and ends with double quotes *)
      Js.Json.string (String.sub s 1 (String.length s - 2))

let write_t ob x =
  let js_t = to_js x in
  Bi_outbuf.add_string ob (Js.Json.stringify js_t)

let to_outbuf ?(std = true) ob x =
  if std then
    if not (is_object_or_array x) then
      json_error "Root is not an object or array"
    else write_t ob x
  else failwith "Only standard JSON syntax is supported by Fojson"

type json_max = t

(* included: type.ml *)

let hex n = Char.chr (if n < 10 then n + 48 else n + 87)

let write_special src start stop ob str =
  Bi_outbuf.add_substring ob src !start (stop - !start);
  Bi_outbuf.add_string ob str;
  start := stop + 1

let write_control_char src start stop ob c =
  Bi_outbuf.add_substring ob src !start (stop - !start);
  let i = Bi_outbuf.alloc ob 6 in
  let dst = ob.o_s in
  Bytes.blit_string "\\u00" 0 dst i 4;
  Bytes.set dst (i + 4) (hex (Char.code c lsr 4));
  Bytes.set dst (i + 5) (hex (Char.code c land 0xf));
  start := stop + 1

let finish_string src start ob =
  try Bi_outbuf.add_substring ob src !start (String.length src - !start)
  with exc ->
    Printf.eprintf "src=%S start=%i len=%i\n%!" src !start
      (String.length src - !start);
    raise exc

let write_string_body ob s =
  let start = ref 0 in
  for i = 0 to String.length s - 1 do
    match s.[i] with
    | '"' -> write_special s start i ob "\\\""
    | '\\' -> write_special s start i ob "\\\\"
    | '\b' -> write_special s start i ob "\\b"
    | '\012' -> write_special s start i ob "\\f"
    | '\n' -> write_special s start i ob "\\n"
    | '\r' -> write_special s start i ob "\\r"
    | '\t' -> write_special s start i ob "\\t"
    | ('\x00' .. '\x1F' | '\x7F') as c -> write_control_char s start i ob c
    | _ -> ()
  done;
  finish_string s start ob

let write_string ob s =
  Bi_outbuf.add_char ob '"';
  write_string_body ob s;
  Bi_outbuf.add_char ob '"'

let json_string_of_string s =
  let ob = Bi_outbuf.create 10 in
  write_string ob s;
  Bi_outbuf.contents ob

let json_string_of_int i = string_of_int i

(*
  Ensure that the float is not printed as an int.
  This is not required by JSON, but useful in order to guarantee
  reversibility.
*)
let float_needs_period s =
  try
    for i = 0 to String.length s - 1 do
      match s.[i] with '0' .. '9' | '-' -> () | _ -> raise Exit
    done;
    true
  with Exit -> false

let write_float ob x =
  match classify_float x with
  | FP_nan -> Bi_outbuf.add_string ob "NaN"
  | FP_infinite ->
      Bi_outbuf.add_string ob (if x > 0. then "Infinity" else "-Infinity")
  | _ ->
      let s1 = Printf.sprintf "%.16g" x in
      let s = if float_of_string s1 = x then s1 else Printf.sprintf "%.17g" x in
      Bi_outbuf.add_string ob s;
      if float_needs_period s then Bi_outbuf.add_string ob ".0"

let json_string_of_float x =
  let ob = Bi_outbuf.create 20 in
  write_float ob x;
  Bi_outbuf.contents ob

let write_std_float ob x =
  match classify_float x with
  | FP_nan -> json_error "NaN value not allowed in standard JSON"
  | FP_infinite ->
      json_error
        ( if x > 0. then "Infinity value not allowed in standard JSON"
        else "-Infinity value not allowed in standard JSON" )
  | _ ->
      let s1 = Printf.sprintf "%.16g" x in
      let s = if float_of_string s1 = x then s1 else Printf.sprintf "%.17g" x in
      Bi_outbuf.add_string ob s;
      if float_needs_period s then Bi_outbuf.add_string ob ".0"

let std_json_string_of_float x =
  let ob = Bi_outbuf.create 20 in
  write_std_float ob x;
  Bi_outbuf.contents ob

let to_string ?buf ?(len = 256) ?std x =
  let ob =
    match buf with
    | None -> Bi_outbuf.create len
    | Some ob ->
        Bi_outbuf.clear ob;
        ob
  in
  to_outbuf ?std ob x;
  let s = Bi_outbuf.contents ob in
  Bi_outbuf.clear ob;
  s

let to_channel ?buf ?len ?std oc x =
  let ob =
    match buf with
    | None -> Bi_outbuf.create_channel_writer ?len oc
    | Some ob -> ob
  in
  to_outbuf ?std ob x;
  Bi_outbuf.flush_channel_writer ob

let to_output ?buf ?len ?std out x =
  let ob =
    match buf with
    | None -> Bi_outbuf.create_output_writer ?len out
    | Some ob -> ob
  in
  to_outbuf ?std ob x;
  Bi_outbuf.flush_output_writer ob

let to_file ?len ?std file x =
  let oc = open_out file in
  try
    to_channel ?len ?std oc x;
    close_out oc
  with e ->
    close_out_noerr oc;
    raise e

let stream_to_outbuf ?std ob st = Stream.iter (to_outbuf ?std ob) st

let stream_to_string ?buf ?(len = 256) ?std st =
  let ob =
    match buf with
    | None -> Bi_outbuf.create len
    | Some ob ->
        Bi_outbuf.clear ob;
        ob
  in
  stream_to_outbuf ?std ob st;
  let s = Bi_outbuf.contents ob in
  Bi_outbuf.clear ob;
  s

let stream_to_channel ?buf ?len ?std oc st =
  let ob =
    match buf with
    | None -> Bi_outbuf.create_channel_writer ?len oc
    | Some ob -> ob
  in
  stream_to_outbuf ?std ob st;
  Bi_outbuf.flush_channel_writer ob

let stream_to_file ?len ?std file st =
  let oc = open_out file in
  try
    stream_to_channel ?len ?std oc st;
    close_out oc
  with e ->
    close_out_noerr oc;
    raise e

let rec sort = function
  | `Assoc l ->
      let l = List.rev (List.rev_map (fun (k, v) -> (k, sort v)) l) in
      `Assoc (List.stable_sort (fun (a, _) (b, _) -> String.compare a b) l)
  | `List l -> `List (List.rev (List.rev_map sort l))
  | `Tuple l -> `Tuple (List.rev (List.rev_map sort l))
  | `Variant (k, Some v) as x ->
      let v' = sort v in
      if v == v' then x else `Variant (k, Some v')
  | x -> x

let rec pp fmt = function
  | `Null -> Format.pp_print_string fmt "`Null"
  | `Bool x ->
      Format.fprintf fmt "`Bool (@[<hov>";
      Format.fprintf fmt "%B" x;
      Format.fprintf fmt "@])"
  | `Int x ->
      Format.fprintf fmt "`Int (@[<hov>";
      Format.fprintf fmt "%d" x;
      Format.fprintf fmt "@])"
  | `Intlit x ->
      Format.fprintf fmt "`Intlit (@[<hov>";
      Format.fprintf fmt "%S" x;
      Format.fprintf fmt "@])"
  | `Float x ->
      Format.fprintf fmt "`Float (@[<hov>";
      Format.fprintf fmt "%F" x;
      Format.fprintf fmt "@])"
  | `Floatlit x ->
      Format.fprintf fmt "`Floatlit (@[<hov>";
      Format.fprintf fmt "%S" x;
      Format.fprintf fmt "@])"
  | `String x ->
      Format.fprintf fmt "`String (@[<hov>";
      Format.fprintf fmt "%S" x;
      Format.fprintf fmt "@])"
  | `Stringlit x ->
      Format.fprintf fmt "`Stringlit (@[<hov>";
      Format.fprintf fmt "%S" x;
      Format.fprintf fmt "@])"
  | `Assoc xs ->
      Format.fprintf fmt "`Assoc (@[<hov>";
      Format.fprintf fmt "@[<2>[";
      ignore
        (List.fold_left
           (fun sep (key, value) ->
             if sep then Format.fprintf fmt ";@ ";
             Format.fprintf fmt "(@[";
             Format.fprintf fmt "%S" key;
             Format.fprintf fmt ",@ ";
             pp fmt value;
             Format.fprintf fmt "@])";
             true)
           false xs);
      Format.fprintf fmt "@,]@]";
      Format.fprintf fmt "@])"
  | `List xs ->
      Format.fprintf fmt "`List (@[<hov>";
      Format.fprintf fmt "@[<2>[";
      ignore
        (List.fold_left
           (fun sep x ->
             if sep then Format.fprintf fmt ";@ ";
             pp fmt x;
             true)
           false xs);
      Format.fprintf fmt "@,]@]";
      Format.fprintf fmt "@])"
  | `Tuple tup ->
      Format.fprintf fmt "`Tuple (@[<hov>";
      Format.fprintf fmt "@[<2>[";
      ignore
        (List.fold_left
           (fun sep e ->
             if sep then Format.fprintf fmt ";@ ";
             pp fmt e;
             true)
           false tup);
      Format.fprintf fmt "@,]@]";
      Format.fprintf fmt "@])"
  | `Variant (name, value) ->
      Format.fprintf fmt "`Variant (@[<hov>";
      Format.fprintf fmt "(@[";
      Format.fprintf fmt "%S" name;
      Format.fprintf fmt ",@ ";
      ( match value with
      | None -> Format.pp_print_string fmt "None"
      | Some x ->
          Format.pp_print_string fmt "(Some ";
          pp fmt x;
          Format.pp_print_string fmt ")" );
      Format.fprintf fmt "@])";
      Format.fprintf fmt "@])"

let show x = Format.asprintf "%a" pp x

let rec equal a b =
  match (a, b) with
  | `Null, `Null -> true
  | `Bool a, `Bool b -> a = b
  | `Int a, `Int b -> a = b
  | `Intlit a, `Intlit b -> a = b
  | `Float a, `Float b -> a = b
  | `Floatlit a, `Floatlit b -> a = b
  | `String a, `String b -> a = b
  | `Stringlit a, `Stringlit b -> a = b
  | `Assoc xs, `Assoc ys -> (
      let compare_keys (key, _) (key', _) = String.compare key key' in
      let xs = List.stable_sort compare_keys xs in
      let ys = List.stable_sort compare_keys ys in
      match
        List.for_all2
          (fun (key, value) (key', value') ->
            match key = key' with false -> false | true -> equal value value')
          xs ys
      with
      | result -> result
      | exception Invalid_argument _ ->
          (* the lists were of different lengths, thus unequal *)
          false )
  | `Tuple xs, `Tuple ys | `List xs, `List ys -> (
      match List.for_all2 equal xs ys with
      | result -> result
      | exception Invalid_argument _ ->
          (* the lists were of different lengths, thus unequal *)
          false )
  | `Variant (name, value), `Variant (name', value') -> (
      match name = name' with
      | false -> false
      | true -> (
          match (value, value') with
          | None, None -> true
          | Some x, Some y -> equal x y
          | _ -> false ) )
  | _ -> false

module Pretty = struct
  open Printf

  let array = Easy_format.list

  let record = Easy_format.list

  let tuple =
    {
      Easy_format.list with
      space_after_opening = false;
      space_before_closing = false;
      align_closing = false;
    }

  let variant = { Easy_format.list with space_before_closing = false }

  let rec format std (x : t) =
    match x with
    | `Null -> Easy_format.Atom ("null", Easy_format.atom)
    | `Bool x ->
        Easy_format.Atom ((if x then "true" else "false"), Easy_format.atom)
    | `Int x -> Easy_format.Atom (json_string_of_int x, Easy_format.atom)
    | `Float x ->
        let s =
          if std then std_json_string_of_float x else json_string_of_float x
        in
        Easy_format.Atom (s, Easy_format.atom)
    | `String s -> Easy_format.Atom (json_string_of_string s, Easy_format.atom)
    | `Intlit s | `Floatlit s | `Stringlit s ->
        Easy_format.Atom (s, Easy_format.atom)
    | `List [] -> Easy_format.Atom ("[]", Easy_format.atom)
    | `List l ->
        Easy_format.List (("[", ",", "]", array), List.map (format std) l)
    | `Assoc [] -> Easy_format.Atom ("{}", Easy_format.atom)
    | `Assoc l ->
        Easy_format.List (("{", ",", "}", record), List.map (format_field std) l)
    | `Tuple l ->
        if std then format std (`List l)
        else if l = [] then Easy_format.Atom ("()", Easy_format.atom)
        else Easy_format.List (("(", ",", ")", tuple), List.map (format std) l)
    | `Variant (s, None) ->
        if std then format std (`String s)
        else
          Easy_format.Atom
            ("<" ^ json_string_of_string s ^ ">", Easy_format.atom)
    | `Variant (s, Some x) ->
        if std then format std (`List [ `String s; x ])
        else
          let op = "<" ^ json_string_of_string s ^ ":" in
          Easy_format.List ((op, "", ">", variant), [ format std x ])

  and format_field std (name, x) =
    let s = sprintf "%s:" (json_string_of_string name) in
    Easy_format.Label
      ((Easy_format.Atom (s, Easy_format.atom), Easy_format.label), format std x)

  let format ?(std = true) x =
    if std && not (is_object_or_array x) then
      json_error
        "Root is not an object or array as requested by the JSON standard"
    else format std (x :> t)

  let to_string ?std x = Easy_format.Pretty.to_string (format ?std x)

  let to_channel ?std oc x = Easy_format.Pretty.to_channel oc (format ?std x)
end

let pretty_format ?std (x : t) = Pretty.format ?std (x :> json_max)

let pretty_print ?std out (x : t) =
  Easy_format.Pretty.to_formatter out (pretty_format ?std x)

let pretty_to_string ?std (x : t) = Pretty.to_string ?std (x :> json_max)

let pretty_to_channel ?std oc (x : t) = Pretty.to_channel ?std oc (x :> json_max)

module Basic = struct
  (** {3 Type of the JSON tree} *)

  type t =
    [ `Null
    | `Bool of bool
    | `Int of int
    | `Float of float
    | `String of string
    | `Assoc of (string * t) list
    | `List of t list ]
  (**
All possible cases defined in Yojson:
- `Null: JSON null
- `Bool of bool: JSON boolean
- `Int of int: JSON number without decimal point or exponent.
- `Intlit of string: JSON number without decimal point or exponent,
	    preserved as a string.
- `Float of float: JSON number, Infinity, -Infinity or NaN.
- `Floatlit of string: JSON number, Infinity, -Infinity or NaN,
	    preserved as a string.
- `String of string: JSON string. Bytes in the range 128-255 are preserved
	    as-is without encoding validation for both reading
	    and writing.
- `Stringlit of string: JSON string literal including the double quotes.
- `Assoc of (string * json) list: JSON object.
- `List of json list: JSON array.
- `Tuple of json list: Tuple (non-standard extension of JSON).
	    Syntax: [("abc", 123)].
- `Variant of (string * json option): Variant (non-standard extension of JSON).
	    Syntax: [<"Foo">] or [<"Bar":123>].
*)

  type json = t
  (**
 * Compatibility type alias for type `t`
 *)

  let rec to_js (x : t) =
    match x with
    | `Null -> Js.Json.null
    | `Bool b -> Js.Json.boolean b
    | `Int i -> js_json_int i
    | `Float f -> Js.Json.number f
    | `String s -> Js.Json.string s
    | `Assoc pairs ->
        Js.Dict.fromList
          ((List.map (fun ((k : string), v) -> (k, to_js v))) pairs)
        |> Js.Json.object_
    | `List xs -> Js.Json.array (List.map to_js xs |> Array.of_list)

  let write_t ob x =
    let js_t = to_js x in
    Bi_outbuf.add_string ob (Js.Json.stringify js_t)

  let to_outbuf ?(std = true) ob x =
    if std then
      if not (is_object_or_array x) then
        json_error "Root is not an object or array"
      else write_t ob x
    else failwith "Only standard JSON syntax is supported by Fojson"

  let to_string ?buf ?(len = 256) ?std x =
    let ob =
      match buf with
      | None -> Bi_outbuf.create len
      | Some ob ->
          Bi_outbuf.clear ob;
          ob
    in
    to_outbuf ?std ob x;
    let s = Bi_outbuf.contents ob in
    Bi_outbuf.clear ob;
    s

  let to_channel ?buf ?len ?std oc x =
    let ob =
      match buf with
      | None -> Bi_outbuf.create_channel_writer ?len oc
      | Some ob -> ob
    in
    to_outbuf ?std ob x;
    Bi_outbuf.flush_channel_writer ob

  let to_output ?buf ?len ?std out x =
    let ob =
      match buf with
      | None -> Bi_outbuf.create_output_writer ?len out
      | Some ob -> ob
    in
    to_outbuf ?std ob x;
    Bi_outbuf.flush_output_writer ob

  let to_file ?len ?std file x =
    let oc = open_out file in
    try
      to_channel ?len ?std oc x;
      close_out oc
    with e ->
      close_out_noerr oc;
      raise e

  let stream_to_outbuf ?std ob st = Stream.iter (to_outbuf ?std ob) st

  let stream_to_string ?buf ?(len = 256) ?std st =
    let ob =
      match buf with
      | None -> Bi_outbuf.create len
      | Some ob ->
          Bi_outbuf.clear ob;
          ob
    in
    stream_to_outbuf ?std ob st;
    let s = Bi_outbuf.contents ob in
    Bi_outbuf.clear ob;
    s

  let stream_to_channel ?buf ?len ?std oc st =
    let ob =
      match buf with
      | None -> Bi_outbuf.create_channel_writer ?len oc
      | Some ob -> ob
    in
    stream_to_outbuf ?std ob st;
    Bi_outbuf.flush_channel_writer ob

  let stream_to_file ?len ?std file st =
    let oc = open_out file in
    try
      stream_to_channel ?len ?std oc st;
      close_out oc
    with e ->
      close_out_noerr oc;
      raise e

  let rec sort = function
    | `Assoc l ->
        let l = List.rev (List.rev_map (fun (k, v) -> (k, sort v)) l) in
        `Assoc (List.stable_sort (fun (a, _) (b, _) -> String.compare a b) l)
    | `List l -> `List (List.rev (List.rev_map sort l))
    | x -> x

  let rec pp fmt = function
    | `Null -> Format.pp_print_string fmt "`Null"
    | `Bool x ->
        Format.fprintf fmt "`Bool (@[<hov>";
        Format.fprintf fmt "%B" x;
        Format.fprintf fmt "@])"
    | `Int x ->
        Format.fprintf fmt "`Int (@[<hov>";
        Format.fprintf fmt "%d" x;
        Format.fprintf fmt "@])"
    | `Float x ->
        Format.fprintf fmt "`Float (@[<hov>";
        Format.fprintf fmt "%F" x;
        Format.fprintf fmt "@])"
    | `String x ->
        Format.fprintf fmt "`String (@[<hov>";
        Format.fprintf fmt "%S" x;
        Format.fprintf fmt "@])"
    | `Assoc xs ->
        Format.fprintf fmt "`Assoc (@[<hov>";
        Format.fprintf fmt "@[<2>[";
        ignore
          (List.fold_left
             (fun sep (key, value) ->
               if sep then Format.fprintf fmt ";@ ";
               Format.fprintf fmt "(@[";
               Format.fprintf fmt "%S" key;
               Format.fprintf fmt ",@ ";
               pp fmt value;
               Format.fprintf fmt "@])";
               true)
             false xs);
        Format.fprintf fmt "@,]@]";
        Format.fprintf fmt "@])"
    | `List xs ->
        Format.fprintf fmt "`List (@[<hov>";
        Format.fprintf fmt "@[<2>[";
        ignore
          (List.fold_left
             (fun sep x ->
               if sep then Format.fprintf fmt ";@ ";
               pp fmt x;
               true)
             false xs);
        Format.fprintf fmt "@,]@]";
        Format.fprintf fmt "@])"

  let show x = Format.asprintf "%a" pp x

  let rec equal a b =
    match (a, b) with
    | `Null, `Null -> true
    | `Bool a, `Bool b -> a = b
    | `Int a, `Int b -> a = b
    | `Float a, `Float b -> a = b
    | `String a, `String b -> a = b
    | `Assoc xs, `Assoc ys -> (
        let compare_keys (key, _) (key', _) = String.compare key key' in
        let xs = List.stable_sort compare_keys xs in
        let ys = List.stable_sort compare_keys ys in
        match
          List.for_all2
            (fun (key, value) (key', value') ->
              match key = key' with
              | false -> false
              | true -> equal value value')
            xs ys
        with
        | result -> result
        | exception Invalid_argument _ ->
            (* the lists were of different lengths, thus unequal *)
            false )
    | `List xs, `List ys -> (
        match List.for_all2 equal xs ys with
        | result -> result
        | exception Invalid_argument _ ->
            (* the lists were of different lengths, thus unequal *)
            false )
    | _ -> false

  let pretty_format ?std (x : t) = Pretty.format ?std (x :> json_max)

  let pretty_print ?std out (x : t) =
    Easy_format.Pretty.to_formatter out (pretty_format ?std x)

  let pretty_to_string ?std (x : t) = Pretty.to_string ?std (x :> json_max)

  let pretty_to_channel ?std oc (x : t) =
    Pretty.to_channel ?std oc (x :> json_max)

  let from_string ?buf:_ ?fname:_ ?lnum:_ s =
    (* In case the original string is invalid and processing would change that *)
    let _js_unprocessed = Js.Json.parseExn s in
    let processed =
      s |> escape_at_symbol |> wrap_ints |> unescape_exclamation_at
    in
    from_processed_js (Js.Json.parseExn processed)

  let from_channel ?buf ?fname ?lnum ic =
    try
      let s = really_input_string ic (in_channel_length ic) in
      from_string ?buf ?fname ?lnum s
    with End_of_input -> json_error "Blank input data"

  let from_file ?buf ?fname ?lnum file =
    let ic = open_in file in
    try
      let x = from_channel ?buf ?fname ?lnum ic in
      close_in ic;
      x
    with e ->
      close_in_noerr ic;
      raise e

  exception Finally of exn * exn

  type json_line = [ `Json of t | `Exn of exn ]

  let linestream_from_channel ?buf ?(fin = fun () -> ()) ?fname
      ?lnum:(lnum0 = 1) ic =
    let buf =
      match buf with None -> Some (Bi_outbuf.create 256) | Some _ -> buf
    in
    let f i =
      try
        let line = input_line ic in
        let lnum = lnum0 + i in
        Some (`Json (from_string ?buf ?fname ~lnum line))
      with
      | End_of_file ->
          fin ();
          None
      | e -> Some (`Exn e)
    in
    Stream.from f

  let linestream_from_file ?buf ?fname ?lnum file =
    let ic = open_in file in
    let fin () = close_in ic in
    let fname = match fname with None -> Some file | x -> x in
    linestream_from_channel ?buf ~fin ?fname ?lnum ic

  let prettify ?std s = pretty_to_string ?std (from_string s)

  let compact ?std:_ s = to_string (from_string s)

  module Util = struct
    exception Type_error of string * t

    let typeof = function
      | `Assoc _ -> "object"
      | `Bool _ -> "bool"
      | `Float _ -> "float"
      | `Int _ -> "int"
      | `List _ -> "array"
      | `Null -> "null"
      | `String _ -> "string"
      | `Intlit _ -> "intlit"
      | `Tuple _ -> "tuple"
      | `Variant _ -> "variant"

    let typerr msg js = raise (Type_error (msg ^ typeof js, js))

    exception Undefined of string * t

    let ( |> ) = ( |> )

    let assoc name obj = try List.assoc name obj with Not_found -> `Null

    let member name = function
      | `Assoc obj -> assoc name obj
      | js -> typerr ("Can't get member '" ^ name ^ "' of non-object type ") js

    let index i = function
      | `List l as js ->
          let len = List.length l in
          let wrapped_index = if i < 0 then len + i else i in
          if wrapped_index < 0 || wrapped_index >= len then
            raise
              (Undefined ("Index " ^ string_of_int i ^ " out of bounds", js))
          else List.nth l wrapped_index
      | js ->
          typerr
            ("Can't get index " ^ string_of_int i ^ " of non-array type ")
            js

    let map f = function
      | `List l -> `List (List.map f l)
      | js -> typerr "Can't map function over non-array type " js

    let to_assoc = function
      | `Assoc obj -> obj
      | js -> typerr "Expected object, got " js

    let to_option f = function `Null -> None | x -> Some (f x)

    let to_bool = function
      | `Bool b -> b
      | js -> typerr "Expected bool, got " js

    let to_bool_option = function
      | `Bool b -> Some b
      | `Null -> None
      | js -> typerr "Expected bool or null, got " js

    let to_number = function
      | `Int i -> float i
      | `Float f -> f
      | js -> typerr "Expected number, got " js

    let to_number_option = function
      | `Int i -> Some (float i)
      | `Float f -> Some f
      | `Null -> None
      | js -> typerr "Expected number or null, got " js

    let to_float = function
      | `Float f -> f
      | js -> typerr "Expected float, got " js

    let to_float_option = function
      | `Float f -> Some f
      | `Null -> None
      | js -> typerr "Expected float or null, got " js

    let to_int = function `Int i -> i | js -> typerr "Expected int, got " js

    let to_int_option = function
      | `Int i -> Some i
      | `Null -> None
      | js -> typerr "Expected int or null, got " js

    let to_list = function
      | `List l -> l
      | js -> typerr "Expected array, got " js

    let to_string = function
      | `String s -> s
      | js -> typerr "Expected string, got " js

    let to_string_option = function
      | `String s -> Some s
      | `Null -> None
      | js -> typerr "Expected string or null, got " js

    let convert_each f = function
      | `List l -> List.map f l
      | js -> typerr "Can't convert each element of non-array type " js

    let rec rev_filter_map f acc l =
      match l with
      | [] -> acc
      | x :: tl -> (
          match f x with
          | None -> rev_filter_map f acc tl
          | Some y -> rev_filter_map f (y :: acc) tl )

    let filter_map f l = List.rev (rev_filter_map f [] l)

    let rec rev_flatten acc l =
      match l with
      | [] -> acc
      | x :: tl -> (
          match x with
          | `List l2 -> rev_flatten (List.rev_append l2 acc) tl
          | _ -> rev_flatten acc tl )

    let flatten l = List.rev (rev_flatten [] l)

    let filter_index i l =
      filter_map
        (function
          | `List l -> ( try Some (List.nth l i) with _ -> None ) | _ -> None)
        l

    let filter_list l = filter_map (function `List l -> Some l | _ -> None) l

    let filter_member k l =
      filter_map
        (function
          | `Assoc l -> ( try Some (List.assoc k l) with _ -> None )
          | _ -> None)
        l

    let filter_assoc l =
      filter_map (function `Assoc l -> Some l | _ -> None) l

    let filter_bool l = filter_map (function `Bool x -> Some x | _ -> None) l

    let filter_int l = filter_map (function `Int x -> Some x | _ -> None) l

    let filter_float l =
      filter_map (function `Float x -> Some x | _ -> None) l

    let filter_number l =
      filter_map
        (function `Int x -> Some (float x) | `Float x -> Some x | _ -> None)
        l

    let filter_string l =
      filter_map (function `String x -> Some x | _ -> None) l

    let keys o = to_assoc o |> List.map (fun (key, _) -> key)

    let values o = to_assoc o |> List.map (fun (_, value) -> value)

    let combine (first : t) (second : t) =
      match (first, second) with
      | `Assoc a, `Assoc b -> (`Assoc (a @ b) : t)
      | _a, _b -> raise (Invalid_argument "Expected two objects, check inputs")
  end
end

module Safe = struct
  (** {3 Type of the JSON tree} *)

  type t =
    [ `Null
    | `Bool of bool
    | `Int of int
    | `Intlit of string
    | `Float of float
    | `String of string
    | `Assoc of (string * t) list
    | `List of t list
    | `Tuple of t list
    | `Variant of string * t option ]
  (**
All possible cases defined in Yojson:
- `Null: JSON null
- `Bool of bool: JSON boolean
- `Int of int: JSON number without decimal point or exponent.
- `Intlit of string: JSON number without decimal point or exponent,
	    preserved as a string.
- `Float of float: JSON number, Infinity, -Infinity or NaN.
- `Floatlit of string: JSON number, Infinity, -Infinity or NaN,
	    preserved as a string.
- `String of string: JSON string. Bytes in the range 128-255 are preserved
	    as-is without encoding validation for both reading
	    and writing.
- `Stringlit of string: JSON string literal including the double quotes.
- `Assoc of (string * json) list: JSON object.
- `List of json list: JSON array.
- `Tuple of json list: Tuple (non-standard extension of JSON).
	    Syntax: [("abc", 123)].
- `Variant of (string * json option): Variant (non-standard extension of JSON).
	    Syntax: [<"Foo">] or [<"Bar":123>].
*)

  type json = t
  (**
 * Compatibility type alias for type `t`
 *)

  (*
  Note to adventurers: ocamldoc does not support inline comments
  on each polymorphic variant, and cppo doesn't allow to concatenate
  comments, so it would be complicated to document only the
  cases that are preserved by cppo in the type definition.
*)

  let rec to_basic : t -> Basic.t = function
    | (`Null | `Bool _ | `Int _ | `Float _ | `String _) as x -> x
    | `Intlit s -> `String s
    | `List l | `Tuple l -> `List (List.rev (List.rev_map to_basic l))
    | `Assoc l ->
        `Assoc (List.rev (List.rev_map (fun (k, v) -> (k, to_basic v)) l))
    | `Variant (k, None) -> `String k
    | `Variant (k, Some v) -> `List [ `String k; to_basic v ]

  let rec to_js (x : t) =
    match x with
    | `Null -> Js.Json.null
    | `Bool b -> Js.Json.boolean b
    | `Int i -> js_json_int i
    | `Intlit i_string -> js_json_int (int_of_string i_string)
    | `Float f -> Js.Json.number f
    | `String s -> Js.Json.string s
    | `Assoc pairs ->
        Js.Dict.fromList
          ((List.map (fun ((k : string), v) -> (k, to_js v))) pairs)
        |> Js.Json.object_
    | `List xs -> Js.Json.array (List.map to_js xs |> Array.of_list)
    | `Tuple xs -> Js.Json.array (List.map to_js xs |> Array.of_list)
    | `Variant (k, v_opt) -> (
        match v_opt with
        | Some v -> to_js (`Tuple [ `String k; v ])
        | None -> to_js (`String k) )

  let write_t ob x =
    let js_t = to_js x in
    Bi_outbuf.add_string ob (Js.Json.stringify js_t)

  let to_outbuf ?(std = true) ob x =
    if std then
      if not (is_object_or_array x) then
        json_error "Root is not an object or array"
      else write_t ob x
    else failwith "Only standard JSON syntax is supported by Fojson"

  let to_string ?buf ?(len = 256) ?std x =
    let ob =
      match buf with
      | None -> Bi_outbuf.create len
      | Some ob ->
          Bi_outbuf.clear ob;
          ob
    in
    to_outbuf ?std ob x;
    let s = Bi_outbuf.contents ob in
    Bi_outbuf.clear ob;
    s

  let to_channel ?buf ?len ?std oc x =
    let ob =
      match buf with
      | None -> Bi_outbuf.create_channel_writer ?len oc
      | Some ob -> ob
    in
    to_outbuf ?std ob x;
    Bi_outbuf.flush_channel_writer ob

  let to_output ?buf ?len ?std out x =
    let ob =
      match buf with
      | None -> Bi_outbuf.create_output_writer ?len out
      | Some ob -> ob
    in
    to_outbuf ?std ob x;
    Bi_outbuf.flush_output_writer ob

  let to_file ?len ?std file x =
    let oc = open_out file in
    try
      to_channel ?len ?std oc x;
      close_out oc
    with e ->
      close_out_noerr oc;
      raise e

  let stream_to_outbuf ?std ob st = Stream.iter (to_outbuf ?std ob) st

  let stream_to_string ?buf ?(len = 256) ?std st =
    let ob =
      match buf with
      | None -> Bi_outbuf.create len
      | Some ob ->
          Bi_outbuf.clear ob;
          ob
    in
    stream_to_outbuf ?std ob st;
    let s = Bi_outbuf.contents ob in
    Bi_outbuf.clear ob;
    s

  let stream_to_channel ?buf ?len ?std oc st =
    let ob =
      match buf with
      | None -> Bi_outbuf.create_channel_writer ?len oc
      | Some ob -> ob
    in
    stream_to_outbuf ?std ob st;
    Bi_outbuf.flush_channel_writer ob

  let stream_to_file ?len ?std file st =
    let oc = open_out file in
    try
      stream_to_channel ?len ?std oc st;
      close_out oc
    with e ->
      close_out_noerr oc;
      raise e

  let rec sort = function
    | `Assoc l ->
        let l = List.rev (List.rev_map (fun (k, v) -> (k, sort v)) l) in
        `Assoc (List.stable_sort (fun (a, _) (b, _) -> String.compare a b) l)
    | `List l -> `List (List.rev (List.rev_map sort l))
    | `Tuple l -> `Tuple (List.rev (List.rev_map sort l))
    | `Variant (k, Some v) as x ->
        let v' = sort v in
        if v == v' then x else `Variant (k, Some v')
    | x -> x

  let rec pp fmt = function
    | `Null -> Format.pp_print_string fmt "`Null"
    | `Bool x ->
        Format.fprintf fmt "`Bool (@[<hov>";
        Format.fprintf fmt "%B" x;
        Format.fprintf fmt "@])"
    | `Int x ->
        Format.fprintf fmt "`Int (@[<hov>";
        Format.fprintf fmt "%d" x;
        Format.fprintf fmt "@])"
    | `Intlit x ->
        Format.fprintf fmt "`Intlit (@[<hov>";
        Format.fprintf fmt "%S" x;
        Format.fprintf fmt "@])"
    | `Float x ->
        Format.fprintf fmt "`Float (@[<hov>";
        Format.fprintf fmt "%F" x;
        Format.fprintf fmt "@])"
    | `String x ->
        Format.fprintf fmt "`String (@[<hov>";
        Format.fprintf fmt "%S" x;
        Format.fprintf fmt "@])"
    | `Assoc xs ->
        Format.fprintf fmt "`Assoc (@[<hov>";
        Format.fprintf fmt "@[<2>[";
        ignore
          (List.fold_left
             (fun sep (key, value) ->
               if sep then Format.fprintf fmt ";@ ";
               Format.fprintf fmt "(@[";
               Format.fprintf fmt "%S" key;
               Format.fprintf fmt ",@ ";
               pp fmt value;
               Format.fprintf fmt "@])";
               true)
             false xs);
        Format.fprintf fmt "@,]@]";
        Format.fprintf fmt "@])"
    | `List xs ->
        Format.fprintf fmt "`List (@[<hov>";
        Format.fprintf fmt "@[<2>[";
        ignore
          (List.fold_left
             (fun sep x ->
               if sep then Format.fprintf fmt ";@ ";
               pp fmt x;
               true)
             false xs);
        Format.fprintf fmt "@,]@]";
        Format.fprintf fmt "@])"
    | `Tuple tup ->
        Format.fprintf fmt "`Tuple (@[<hov>";
        Format.fprintf fmt "@[<2>[";
        ignore
          (List.fold_left
             (fun sep e ->
               if sep then Format.fprintf fmt ";@ ";
               pp fmt e;
               true)
             false tup);
        Format.fprintf fmt "@,]@]";
        Format.fprintf fmt "@])"
    | `Variant (name, value) ->
        Format.fprintf fmt "`Variant (@[<hov>";
        Format.fprintf fmt "(@[";
        Format.fprintf fmt "%S" name;
        Format.fprintf fmt ",@ ";
        ( match value with
        | None -> Format.pp_print_string fmt "None"
        | Some x ->
            Format.pp_print_string fmt "(Some ";
            pp fmt x;
            Format.pp_print_string fmt ")" );
        Format.fprintf fmt "@])";
        Format.fprintf fmt "@])"

  let show x = Format.asprintf "%a" pp x

  let rec equal a b =
    match (a, b) with
    | `Null, `Null -> true
    | `Bool a, `Bool b -> a = b
    | `Int a, `Int b -> a = b
    | `Intlit a, `Intlit b -> a = b
    | `Float a, `Float b -> a = b
    | `String a, `String b -> a = b
    | `Assoc xs, `Assoc ys -> (
        let compare_keys (key, _) (key', _) = String.compare key key' in
        let xs = List.stable_sort compare_keys xs in
        let ys = List.stable_sort compare_keys ys in
        match
          List.for_all2
            (fun (key, value) (key', value') ->
              match key = key' with
              | false -> false
              | true -> equal value value')
            xs ys
        with
        | result -> result
        | exception Invalid_argument _ ->
            (* the lists were of different lengths, thus unequal *)
            false )
    | `Tuple xs, `Tuple ys | `List xs, `List ys -> (
        match List.for_all2 equal xs ys with
        | result -> result
        | exception Invalid_argument _ ->
            (* the lists were of different lengths, thus unequal *)
            false )
    | `Variant (name, value), `Variant (name', value') -> (
        match name = name' with
        | false -> false
        | true -> (
            match (value, value') with
            | None, None -> true
            | Some x, Some y -> equal x y
            | _ -> false ) )
    | _ -> false

  let pretty_format ?std (x : t) = Pretty.format ?std (x :> json_max)

  let pretty_print ?std out (x : t) =
    Easy_format.Pretty.to_formatter out (pretty_format ?std x)

  let pretty_to_string ?std (x : t) = Pretty.to_string ?std (x :> json_max)

  let pretty_to_channel ?std oc (x : t) =
    Pretty.to_channel ?std oc (x :> json_max)

  let from_string ?buf:_ ?fname:_ ?lnum:_ s =
    (* In case the original string is invalid and processing would change that *)
    let _js_unprocessed = Js.Json.parseExn s in
    let processed =
      s |> escape_at_symbol |> wrap_ints |> unescape_exclamation_at
    in
    from_processed_js (Js.Json.parseExn processed)

  let from_channel ?buf ?fname ?lnum ic =
    try
      let s = really_input_string ic (in_channel_length ic) in
      from_string ?buf ?fname ?lnum s
    with End_of_input -> json_error "Blank input data"

  let from_file ?buf ?fname ?lnum file =
    let ic = open_in file in
    try
      let x = from_channel ?buf ?fname ?lnum ic in
      close_in ic;
      x
    with e ->
      close_in_noerr ic;
      raise e

  exception Finally of exn * exn

  type json_line = [ `Json of t | `Exn of exn ]

  let linestream_from_channel ?buf ?(fin = fun () -> ()) ?fname
      ?lnum:(lnum0 = 1) ic =
    let buf =
      match buf with None -> Some (Bi_outbuf.create 256) | Some _ -> buf
    in
    let f i =
      try
        let line = input_line ic in
        let lnum = lnum0 + i in
        Some (`Json (from_string ?buf ?fname ~lnum line))
      with
      | End_of_file ->
          fin ();
          None
      | e -> Some (`Exn e)
    in
    Stream.from f

  let linestream_from_file ?buf ?fname ?lnum file =
    let ic = open_in file in
    let fin () = close_in ic in
    let fname = match fname with None -> Some file | x -> x in
    linestream_from_channel ?buf ~fin ?fname ?lnum ic

  let prettify ?std s = pretty_to_string ?std (from_string s)

  let compact ?std:_ s = to_string (from_string s)

  module Util = struct
    exception Type_error of string * t

    let typeof = function
      | `Assoc _ -> "object"
      | `Bool _ -> "bool"
      | `Float _ -> "float"
      | `Int _ -> "int"
      | `List _ -> "array"
      | `Null -> "null"
      | `String _ -> "string"
      | `Intlit _ -> "intlit"
      | `Tuple _ -> "tuple"
      | `Variant _ -> "variant"

    let typerr msg js = raise (Type_error (msg ^ typeof js, js))

    exception Undefined of string * t

    let ( |> ) = ( |> )

    let assoc name obj = try List.assoc name obj with Not_found -> `Null

    let member name = function
      | `Assoc obj -> assoc name obj
      | js -> typerr ("Can't get member '" ^ name ^ "' of non-object type ") js

    let index i = function
      | `List l as js ->
          let len = List.length l in
          let wrapped_index = if i < 0 then len + i else i in
          if wrapped_index < 0 || wrapped_index >= len then
            raise
              (Undefined ("Index " ^ string_of_int i ^ " out of bounds", js))
          else List.nth l wrapped_index
      | js ->
          typerr
            ("Can't get index " ^ string_of_int i ^ " of non-array type ")
            js

    let map f = function
      | `List l -> `List (List.map f l)
      | js -> typerr "Can't map function over non-array type " js

    let to_assoc = function
      | `Assoc obj -> obj
      | js -> typerr "Expected object, got " js

    let to_option f = function `Null -> None | x -> Some (f x)

    let to_bool = function
      | `Bool b -> b
      | js -> typerr "Expected bool, got " js

    let to_bool_option = function
      | `Bool b -> Some b
      | `Null -> None
      | js -> typerr "Expected bool or null, got " js

    let to_number = function
      | `Int i -> float i
      | `Float f -> f
      | js -> typerr "Expected number, got " js

    let to_number_option = function
      | `Int i -> Some (float i)
      | `Float f -> Some f
      | `Null -> None
      | js -> typerr "Expected number or null, got " js

    let to_float = function
      | `Float f -> f
      | js -> typerr "Expected float, got " js

    let to_float_option = function
      | `Float f -> Some f
      | `Null -> None
      | js -> typerr "Expected float or null, got " js

    let to_int = function `Int i -> i | js -> typerr "Expected int, got " js

    let to_int_option = function
      | `Int i -> Some i
      | `Null -> None
      | js -> typerr "Expected int or null, got " js

    let to_list = function
      | `List l -> l
      | js -> typerr "Expected array, got " js

    let to_string = function
      | `String s -> s
      | js -> typerr "Expected string, got " js

    let to_string_option = function
      | `String s -> Some s
      | `Null -> None
      | js -> typerr "Expected string or null, got " js

    let convert_each f = function
      | `List l -> List.map f l
      | js -> typerr "Can't convert each element of non-array type " js

    let rec rev_filter_map f acc l =
      match l with
      | [] -> acc
      | x :: tl -> (
          match f x with
          | None -> rev_filter_map f acc tl
          | Some y -> rev_filter_map f (y :: acc) tl )

    let filter_map f l = List.rev (rev_filter_map f [] l)

    let rec rev_flatten acc l =
      match l with
      | [] -> acc
      | x :: tl -> (
          match x with
          | `List l2 -> rev_flatten (List.rev_append l2 acc) tl
          | _ -> rev_flatten acc tl )

    let flatten l = List.rev (rev_flatten [] l)

    let filter_index i l =
      filter_map
        (function
          | `List l -> ( try Some (List.nth l i) with _ -> None ) | _ -> None)
        l

    let filter_list l = filter_map (function `List l -> Some l | _ -> None) l

    let filter_member k l =
      filter_map
        (function
          | `Assoc l -> ( try Some (List.assoc k l) with _ -> None )
          | _ -> None)
        l

    let filter_assoc l =
      filter_map (function `Assoc l -> Some l | _ -> None) l

    let filter_bool l = filter_map (function `Bool x -> Some x | _ -> None) l

    let filter_int l = filter_map (function `Int x -> Some x | _ -> None) l

    let filter_float l =
      filter_map (function `Float x -> Some x | _ -> None) l

    let filter_number l =
      filter_map
        (function `Int x -> Some (float x) | `Float x -> Some x | _ -> None)
        l

    let filter_string l =
      filter_map (function `String x -> Some x | _ -> None) l

    let keys o = to_assoc o |> List.map (fun (key, _) -> key)

    let values o = to_assoc o |> List.map (fun (_, value) -> value)

    let combine (first : t) (second : t) =
      match (first, second) with
      | `Assoc a, `Assoc b -> (`Assoc (a @ b) : t)
      | _a, _b -> raise (Invalid_argument "Expected two objects, check inputs")
  end
end

module Raw = struct
  (** {3 Type of the JSON tree} *)

  type t =
    [ `Null
    | `Bool of bool
    | `Intlit of string
    | `Floatlit of string
    | `Stringlit of string
    | `Assoc of (string * t) list
    | `List of t list
    | `Tuple of t list
    | `Variant of string * t option ]
  (**
All possible cases defined in Yojson:
- `Null: JSON null
- `Bool of bool: JSON boolean
- `Int of int: JSON number without decimal point or exponent.
- `Intlit of string: JSON number without decimal point or exponent,
	    preserved as a string.
- `Float of float: JSON number, Infinity, -Infinity or NaN.
- `Floatlit of string: JSON number, Infinity, -Infinity or NaN,
	    preserved as a string.
- `String of string: JSON string. Bytes in the range 128-255 are preserved
	    as-is without encoding validation for both reading
	    and writing.
- `Stringlit of string: JSON string literal including the double quotes.
- `Assoc of (string * json) list: JSON object.
- `List of json list: JSON array.
- `Tuple of json list: Tuple (non-standard extension of JSON).
	    Syntax: [("abc", 123)].
- `Variant of (string * json option): Variant (non-standard extension of JSON).
	    Syntax: [<"Foo">] or [<"Bar":123>].
*)

  type json = t
  (**
 * Compatibility type alias for type `t`
 *)

  let rec to_js (x : t) =
    match x with
    | `Null -> Js.Json.null
    | `Bool b -> Js.Json.boolean b
    | `Intlit i_string -> js_json_int (int_of_string i_string)
    | `Floatlit f_string -> js_json_float (float_of_string f_string)
    | `Stringlit s ->
        (* Assume valid, i.e. starts and ends with double quotes *)
        Js.Json.string (String.sub s 1 (String.length s - 2))
    | `Assoc pairs ->
        Js.Dict.fromList
          ((List.map (fun ((k : string), v) -> (k, to_js v))) pairs)
        |> Js.Json.object_
    | `List xs -> Js.Json.array (List.map to_js xs |> Array.of_list)
    | `Tuple xs -> Js.Json.array (List.map to_js xs |> Array.of_list)
    | `Variant (k, v_opt) -> (
        match v_opt with
        | Some v -> to_js (`Tuple [ `Stringlit ("\"" ^ k ^ "\""); v ])
        | None -> to_js (`Stringlit ("\"" ^ k ^ "\"")) )

  let write_t ob x =
    let js_t = to_js x in
    Bi_outbuf.add_string ob (Js.Json.stringify js_t)

  let to_outbuf ?(std = true) ob x =
    if std then
      if not (is_object_or_array x) then
        json_error "Root is not an object or array"
      else write_t ob x
    else failwith "Only standard JSON syntax is supported by Fojson"

  let to_string ?buf ?(len = 256) ?std x =
    let ob =
      match buf with
      | None -> Bi_outbuf.create len
      | Some ob ->
          Bi_outbuf.clear ob;
          ob
    in
    to_outbuf ?std ob x;
    let s = Bi_outbuf.contents ob in
    Bi_outbuf.clear ob;
    s

  let to_channel ?buf ?len ?std oc x =
    let ob =
      match buf with
      | None -> Bi_outbuf.create_channel_writer ?len oc
      | Some ob -> ob
    in
    to_outbuf ?std ob x;
    Bi_outbuf.flush_channel_writer ob

  let to_output ?buf ?len ?std out x =
    let ob =
      match buf with
      | None -> Bi_outbuf.create_output_writer ?len out
      | Some ob -> ob
    in
    to_outbuf ?std ob x;
    Bi_outbuf.flush_output_writer ob

  let to_file ?len ?std file x =
    let oc = open_out file in
    try
      to_channel ?len ?std oc x;
      close_out oc
    with e ->
      close_out_noerr oc;
      raise e

  let stream_to_outbuf ?std ob st = Stream.iter (to_outbuf ?std ob) st

  let stream_to_string ?buf ?(len = 256) ?std st =
    let ob =
      match buf with
      | None -> Bi_outbuf.create len
      | Some ob ->
          Bi_outbuf.clear ob;
          ob
    in
    stream_to_outbuf ?std ob st;
    let s = Bi_outbuf.contents ob in
    Bi_outbuf.clear ob;
    s

  let stream_to_channel ?buf ?len ?std oc st =
    let ob =
      match buf with
      | None -> Bi_outbuf.create_channel_writer ?len oc
      | Some ob -> ob
    in
    stream_to_outbuf ?std ob st;
    Bi_outbuf.flush_channel_writer ob

  let stream_to_file ?len ?std file st =
    let oc = open_out file in
    try
      stream_to_channel ?len ?std oc st;
      close_out oc
    with e ->
      close_out_noerr oc;
      raise e

  let rec sort = function
    | `Assoc l ->
        let l = List.rev (List.rev_map (fun (k, v) -> (k, sort v)) l) in
        `Assoc (List.stable_sort (fun (a, _) (b, _) -> String.compare a b) l)
    | `List l -> `List (List.rev (List.rev_map sort l))
    | `Tuple l -> `Tuple (List.rev (List.rev_map sort l))
    | `Variant (k, Some v) as x ->
        let v' = sort v in
        if v == v' then x else `Variant (k, Some v')
    | x -> x

  let rec pp fmt = function
    | `Null -> Format.pp_print_string fmt "`Null"
    | `Bool x ->
        Format.fprintf fmt "`Bool (@[<hov>";
        Format.fprintf fmt "%B" x;
        Format.fprintf fmt "@])"
    | `Intlit x ->
        Format.fprintf fmt "`Intlit (@[<hov>";
        Format.fprintf fmt "%S" x;
        Format.fprintf fmt "@])"
    | `Floatlit x ->
        Format.fprintf fmt "`Floatlit (@[<hov>";
        Format.fprintf fmt "%S" x;
        Format.fprintf fmt "@])"
    | `Stringlit x ->
        Format.fprintf fmt "`Stringlit (@[<hov>";
        Format.fprintf fmt "%S" x;
        Format.fprintf fmt "@])"
    | `Assoc xs ->
        Format.fprintf fmt "`Assoc (@[<hov>";
        Format.fprintf fmt "@[<2>[";
        ignore
          (List.fold_left
             (fun sep (key, value) ->
               if sep then Format.fprintf fmt ";@ ";
               Format.fprintf fmt "(@[";
               Format.fprintf fmt "%S" key;
               Format.fprintf fmt ",@ ";
               pp fmt value;
               Format.fprintf fmt "@])";
               true)
             false xs);
        Format.fprintf fmt "@,]@]";
        Format.fprintf fmt "@])"
    | `List xs ->
        Format.fprintf fmt "`List (@[<hov>";
        Format.fprintf fmt "@[<2>[";
        ignore
          (List.fold_left
             (fun sep x ->
               if sep then Format.fprintf fmt ";@ ";
               pp fmt x;
               true)
             false xs);
        Format.fprintf fmt "@,]@]";
        Format.fprintf fmt "@])"
    | `Tuple tup ->
        Format.fprintf fmt "`Tuple (@[<hov>";
        Format.fprintf fmt "@[<2>[";
        ignore
          (List.fold_left
             (fun sep e ->
               if sep then Format.fprintf fmt ";@ ";
               pp fmt e;
               true)
             false tup);
        Format.fprintf fmt "@,]@]";
        Format.fprintf fmt "@])"
    | `Variant (name, value) ->
        Format.fprintf fmt "`Variant (@[<hov>";
        Format.fprintf fmt "(@[";
        Format.fprintf fmt "%S" name;
        Format.fprintf fmt ",@ ";
        ( match value with
        | None -> Format.pp_print_string fmt "None"
        | Some x ->
            Format.pp_print_string fmt "(Some ";
            pp fmt x;
            Format.pp_print_string fmt ")" );
        Format.fprintf fmt "@])";
        Format.fprintf fmt "@])"

  let show x = Format.asprintf "%a" pp x

  let rec equal a b =
    match (a, b) with
    | `Null, `Null -> true
    | `Bool a, `Bool b -> a = b
    | `Intlit a, `Intlit b -> a = b
    | `Floatlit a, `Floatlit b -> a = b
    | `Stringlit a, `Stringlit b -> a = b
    | `Assoc xs, `Assoc ys -> (
        let compare_keys (key, _) (key', _) = String.compare key key' in
        let xs = List.stable_sort compare_keys xs in
        let ys = List.stable_sort compare_keys ys in
        match
          List.for_all2
            (fun (key, value) (key', value') ->
              match key = key' with
              | false -> false
              | true -> equal value value')
            xs ys
        with
        | result -> result
        | exception Invalid_argument _ ->
            (* the lists were of different lengths, thus unequal *)
            false )
    | `Tuple xs, `Tuple ys | `List xs, `List ys -> (
        match List.for_all2 equal xs ys with
        | result -> result
        | exception Invalid_argument _ ->
            (* the lists were of different lengths, thus unequal *)
            false )
    | `Variant (name, value), `Variant (name', value') -> (
        match name = name' with
        | false -> false
        | true -> (
            match (value, value') with
            | None, None -> true
            | Some x, Some y -> equal x y
            | _ -> false ) )
    | _ -> false

  let pretty_format ?std (x : t) = Pretty.format ?std (x :> json_max)

  let pretty_print ?std out (x : t) =
    Easy_format.Pretty.to_formatter out (pretty_format ?std x)

  let pretty_to_string ?std (x : t) = Pretty.to_string ?std (x :> json_max)

  let pretty_to_channel ?std oc (x : t) =
    Pretty.to_channel ?std oc (x :> json_max)

  let rec from_processed_js js =
    let open Js.Json in
    match classify js with
    | JSONNull -> `Null
    | JSONTrue -> `Bool true
    | JSONFalse -> `Bool false
    | JSONNumber x -> `Floatlit (Js.Float.toString x)
    | JSONString s ->
        if Js.String.get s 0 = "@" then
          `Intlit (s |> Js.String.sliceToEnd ~from:1)
        else `Stringlit ("\"" ^ s ^ "\"")
    | JSONObject dict ->
        let pairs =
          Js.Dict.entries dict |> Array.to_list
          |> List.map (fun (k, v) -> (k, from_processed_js v))
        in
        `Assoc pairs
    | JSONArray xs -> `List (xs |> Array.to_list |> List.map from_processed_js)

  let from_string ?buf:_ ?fname:_ ?lnum:_ s =
    (* In case the original string is invalid and processing would change that *)
    let _js_unprocessed = Js.Json.parseExn s in
    let processed =
      s |> escape_at_symbol |> wrap_ints |> unescape_exclamation_at
    in
    from_processed_js (Js.Json.parseExn processed)

  let from_channel ?buf ?fname ?lnum ic =
    try
      let s = really_input_string ic (in_channel_length ic) in
      from_string ?buf ?fname ?lnum s
    with End_of_input -> json_error "Blank input data"

  let from_file ?buf ?fname ?lnum file =
    let ic = open_in file in
    try
      let x = from_channel ?buf ?fname ?lnum ic in
      close_in ic;
      x
    with e ->
      close_in_noerr ic;
      raise e

  exception Finally of exn * exn

  type json_line = [ `Json of t | `Exn of exn ]

  let linestream_from_channel ?buf ?(fin = fun () -> ()) ?fname
      ?lnum:(lnum0 = 1) ic =
    let buf =
      match buf with None -> Some (Bi_outbuf.create 256) | Some _ -> buf
    in
    let f i =
      try
        let line = input_line ic in
        let lnum = lnum0 + i in
        Some (`Json (from_string ?buf ?fname ~lnum line))
      with
      | End_of_file ->
          fin ();
          None
      | e -> Some (`Exn e)
    in
    Stream.from f

  let linestream_from_file ?buf ?fname ?lnum file =
    let ic = open_in file in
    let fin () = close_in ic in
    let fname = match fname with None -> Some file | x -> x in
    linestream_from_channel ?buf ~fin ?fname ?lnum ic

  let prettify ?std s = pretty_to_string ?std (from_string s)

  let compact ?std:_ s = to_string (from_string s)
end
