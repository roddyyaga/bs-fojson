# 1 "yojson.cppo.mli"
(**
   The Yojson library provides runtime functions for reading and writing JSON
   data from OCaml. It addresses a few shortcomings of its predecessor
   json-wheel and is about twice as fast (2.7x reading, 1.3x writing; results
   may vary).
   The design goals of Yojson are the following:
   - Reducing inter-package dependencies by the use of polymorphic
   variants for the JSON tree type.
   - Allowing type-aware serializers/deserializers 
   to read and write directly without going through a generic JSON tree,
   for efficiency purposes.
   Readers and writers of all JSON syntaxic elements are provided
   but are undocumented and meant to be used by generated OCaml code.
   - Distinguishing between ints and floats.
   - Providing optional extensions of the JSON syntax.
   These extensions include comments, arbitrary strings,
   optional quotes around field names, tuples and variants.
   
   @author Martin Jambon
   @see <http://json.org> JSON specification
 *)

(** {1 Shared types and functions} *)

# 1 "common.mli"
val version : string

exception Json_error of string

val json_error : string -> 'a


# 27 "yojson.cppo.mli"
(** {1 Basic JSON tree type} *)

module Basic :
sig
(**
   This module supports standard JSON nodes only, i.e. no special syntax
   for variants or tuples as supported by {!Yojson.Safe}.
   Arbitrary integers are not supported as they must all fit within the
   standard OCaml int type (31 or 63 bits depending on the platform).

   The main advantage of this module is its simplicity.
*)

# 1 "type.ml"
(** {3 Type of the JSON tree} *)

type t =
    [
    | `Null
    | `Bool of bool
    
# 8 "type.ml"
    | `Int of int
    
# 14 "type.ml"
    | `Float of float
    
# 20 "type.ml"
    | `String of string
    
# 25 "type.ml"
    | `Assoc of (string * t) list
    | `List of t list
    
# 33 "type.ml"
    ]
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
# 1 "write.mli"
(** {2 JSON writers} *)

val to_string :
  ?buf:Bi_outbuf.t ->
  ?len:int ->
  ?std:bool ->
  t -> string
  (** Write a compact JSON value to a string.
      @param buf allows to reuse an existing buffer created with
      [Bi_outbuf.create]. The buffer is cleared of all contents
      before starting and right before returning.
      @param len initial length of the output buffer.
      @param std use only standard JSON syntax,
      i.e. convert tuples and variants into standard JSON (if applicable),
      refuse to print NaN and infinities,
      require the root node to be either an object or an array.
      Default is [false].
  *)

val to_channel :
  ?buf:Bi_outbuf.t ->
  ?len:int ->
  ?std:bool ->
  out_channel -> t -> unit
  (** Write a compact JSON value to a channel.
      @param buf allows to reuse an existing buffer created with
      [Bi_outbuf.create_channel_writer] on the same channel.
      [buf] is flushed right
      before [to_channel] returns but the [out_channel] is
      not flushed automatically.

      See [to_string] for the role of the other optional arguments. *)

val to_output :
  ?buf:Bi_outbuf.t ->
  ?len:int ->
  ?std:bool ->
  < output : string -> int -> int -> int; .. > -> t -> unit
  (** Write a compact JSON value to an OO channel.
      @param buf allows to reuse an existing buffer created with
      [Bi_outbuf.create_output_writer] on the same channel.
      [buf] is flushed right
      before [to_output] returns but the channel itself is
      not flushed automatically.

      See [to_string] for the role of the other optional arguments. *)

val to_file :
  ?len:int ->
  ?std:bool ->
  string -> t -> unit
  (** Write a compact JSON value to a file.
      See [to_string] for the role of the optional arguments. *)

val to_outbuf :
  ?std:bool ->
  Bi_outbuf.t -> t -> unit
  (** Write a compact JSON value to an existing buffer.
      See [to_string] for the role of the optional argument. *)

val stream_to_string :
  ?buf:Bi_outbuf.t ->
  ?len:int ->
  ?std:bool ->
  t Stream.t -> string
  (** Write a newline-separated sequence of compact one-line JSON values to
      a string.
      See [to_string] for the role of the optional arguments. *)

val stream_to_channel :
  ?buf:Bi_outbuf.t ->
  ?len:int ->
  ?std:bool ->
  out_channel -> t Stream.t -> unit
  (** Write a newline-separated sequence of compact one-line JSON values to
      a channel.
      See [to_channel] for the role of the optional arguments. *)

val stream_to_file :
  ?len:int ->
  ?std:bool ->
  string -> t Stream.t -> unit
  (** Write a newline-separated sequence of compact one-line JSON values to
      a file.
      See [to_string] for the role of the optional arguments. *)

val stream_to_outbuf :
  ?std:bool ->
  Bi_outbuf.t ->
  t Stream.t -> unit
  (** Write a newline-separated sequence of compact one-line JSON values to
      an existing buffer.
      See [to_string] for the role of the optional arguments. *)

val write_t : Bi_outbuf.t -> t -> unit
(** Write the given JSON value to the given buffer.
    Provided as a writer function for atdgen.
*)

(** {2 Miscellaneous} *)

val sort : t -> t
  (** Sort object fields (stable sort, comparing field names
      and treating them as byte sequences) *)


# 1 "monomorphic.mli"
val pp : Format.formatter -> t -> unit
  (** Pretty printer, useful for debugging *)

val show : t -> string
  (** Convert value to string, useful for debugging *)

val equal : t -> t -> bool
  (** [equal a b] is the monomorphic equality.
      Determines whether two JSON values are considered equal. In the case of
      JSON objects, the order of the keys does not matter, except for
      duplicate keys which will be considered equal as long as they are in the
      same input order.
    *)
# 1 "write2.mli"
(** {2 JSON pretty-printing} *)

val pretty_format : ?std:bool -> t -> Easy_format.t
  (** Convert into a pretty-printable tree.
      See [to_string] for the role of the optional [std] argument.

      @see <http://martin.jambon.free.fr/easy-format.html> Easy-format
  *)

val pretty_print : ?std:bool -> Format.formatter -> t -> unit
  (** Pretty-print into a {!Format.formatter}.
      See [to_string] for the role of the optional [std] argument.

      @since 1.3.1 *)

val pretty_to_string : ?std:bool -> t -> string
  (** Pretty-print into a string.
      See [to_string] for the role of the optional [std] argument.
  *)

val pretty_to_channel : ?std:bool -> out_channel -> t -> unit
  (** Pretty-print to a channel.
      See [to_string] for the role of the optional [std] argument.
  *)
# 1 "read.mli"
val prettify : ?std:bool -> string -> string
  (** Combined parser and pretty-printer.
      See [to_string] for the role of the optional [std] argument. *)

val compact : ?std:bool -> string -> string
  (** Combined parser and printer.
      See [to_string] for the role of the optional [std] argument. *)

(** {2 JSON readers} *)

exception Finally of exn * exn
(** Exception describing a failure in both finalizer and parsing. *)

val from_string :
  ?buf:Bi_outbuf.t ->
  ?fname:string ->
  ?lnum:int ->
  string -> t
  (** Read a JSON value from a string.
      @param buf use this buffer at will during parsing instead of creating
      a new one.
      @param fname data file name to be used in error messages. It does
      not have to be a real file.
      @param lnum number of the first line of input. Default is 1.
  *)

val from_channel :
  ?buf:Bi_outbuf.t ->
  ?fname:string ->
  ?lnum:int ->
  in_channel -> t
  (** Read a JSON value from a channel.
      See [from_string] for the meaning of the optional arguments. *)

val from_file :
  ?buf:Bi_outbuf.t ->
  ?fname:string ->
  ?lnum:int ->
  string -> t
  (** Read a JSON value from a file.
      See [from_string] for the meaning of the optional arguments. *)





type json_line = [ `Json of t | `Exn of exn ]
    (** The type of values resulting from a parsing attempt of a JSON value. *)

val linestream_from_channel :
  ?buf:Bi_outbuf.t ->
  ?fin:(unit -> unit) ->
  ?fname:string ->
  ?lnum:int ->
  in_channel -> json_line Stream.t
  (** Input a sequence of JSON values, one per line, from a channel.
      Exceptions raised when reading malformed lines are caught
      and represented using [`Exn].

      See [stream_from_channel] for the meaning of the optional [fin]
      argument.
      See [from_string] for the meaning of the other optional arguments. *)

val linestream_from_file :
  ?buf:Bi_outbuf.t ->
  ?fname:string ->
  ?lnum:int ->
  string -> json_line Stream.t
  (** Input a sequence of JSON values, one per line, from a file.
      Exceptions raised when reading malformed lines are caught
      and represented using [`Exn].

      See [stream_from_channel] for the meaning of the optional [fin]
      argument.
      See [from_string] for the meaning of the other optional arguments. *)

# 48 "yojson.cppo.mli"
module Util :
sig
# 1 "util.mli"
(**
   This module provides combinators for extracting fields from JSON
   values. This approach is recommended for reading a few fields
   from data returned by public APIs. However for more complex applications
   we recommend {{:https://github.com/MyLifeLabs/atdgen}Atdgen}.

   Here is some sample JSON data:
{v
\{
  "id": "398eb027",
  "name": "John Doe",
  "pages": [
    \{
      "id": 1,
      "title": "The Art of Flipping Coins",
      "url": "http://example.com/398eb027/1"
    },
    \{
      "id": 2,
      "deleted": true
    },
    \{
      "id": 3,
      "title": "Artichoke Salad",
      "url": "http://example.com/398eb027/3"
    },
    \{
      "id": 4,
      "title": "Flying Bananas",
      "url": "http://example.com/398eb027/4"
    }
  ]
}
v}

   In order to extract the "id" field, assuming it is mandatory,
   we would use the following OCaml code that operates on single JSON
   nodes:
{v
open Yojson.Basic.Util
...
  let id = json |> member "id" |> to_string in
  ...
v}

   In order to extract all the "title" fields, we would write the following
   OCaml code that operates on lists of JSON nodes, skipping
   undefined nodes and nodes of unexpected type:
{v
open Yojson.Basic.Util

let extract_titles (json : Yojson.Basic.t) : string list =
  [json]
    |> filter_member "pages"
    |> flatten
    |> filter_member "title"
    |> filter_string
v}
*)

exception Type_error of string * t
  (** Raised when the JSON value is not of the correct type to support an
      operation, e.g. [member] on an [`Int]. The string message explains the
      mismatch. *)

exception Undefined of string * t
  (** Raised when the equivalent JavaScript operation on the JSON value would
      return undefined. Currently this only happens when an array index is out
      of bounds. *)

val ( |> ) : 'a -> ('a -> 'b) -> 'b
(** @deprecated Forward pipe operator; useful for composing JSON
    access functions without too many parentheses *)

val keys : t -> string list
  (** Returns all the key names in the given JSON object *)

val values : t -> t list
  (** Return all the value in the given JSON object *)

val combine : t -> t -> t
  (** Combine two JSON Objects together *)

val member : string -> t -> t
  (** [member k obj] returns the value associated with the key [k] in the JSON
      object [obj], or [`Null] if [k] is not present in [obj]. *)

val index : int -> t -> t
  (** [index i arr] returns the value at index [i] in the JSON array [arr].
      Negative indices count from the end of the list (so -1 is the last
      element). *)

val map : (t -> t) -> t -> t
  (** [map f arr] calls the function [f] on each element of the JSON array
      [arr], and returns a JSON array containing the results. *)

val to_assoc : t -> (string * t) list
  (** Extract the items of a JSON object or raise [Type_error]. *)

val to_option : (t -> 'a) -> t -> 'a option
  (** Return [None] if the JSON value is null or map the JSON value
      to [Some] value using the provided function. *)

val to_bool : t -> bool
  (** Extract a boolean value or raise [Type_error]. *)

val to_bool_option : t -> bool option
  (** Extract [Some] boolean value,
      return [None] if the value is null,
      or raise [Type_error] otherwise. *)

val to_number : t -> float
  (** Extract a number or raise [Type_error]. *)

val to_number_option : t -> float option
  (** Extract [Some] number,
      return [None] if the value is null,
      or raise [Type_error] otherwise. *)

val to_float : t -> float
  (** Extract a float value or raise [Type_error].
      [to_number] is generally preferred as it also works with int literals. *)

val to_float_option : t -> float option
  (** Extract [Some] float value,
      return [None] if the value is null,
      or raise [Type_error] otherwise.
      [to_number_option] is generally preferred as it also works
      with int literals. *)

val to_int : t -> int
  (** Extract an int from a JSON int or raise [Type_error]. *)

val to_int_option : t -> int option
  (** Extract [Some] int from a JSON int,
      return [None] if the value is null,
      or raise [Type_error] otherwise. *)

val to_list : t -> t list
  (** Extract a list from JSON array or raise [Type_error]. *)

val to_string : t -> string
  (** Extract a string from a JSON string or raise [Type_error]. *)

val to_string_option : t -> string option
  (** Extract [Some] string from a JSON string,
      return [None] if the value is null,
      or raise [Type_error] otherwise. *)

val convert_each : (t -> 'a) -> t -> 'a list
  (** The conversion functions above cannot be used with [map], because they do
      not return JSON values. This convenience function [convert_each to_f arr]
      is equivalent to [List.map to_f (to_list arr)]. *)


(** {3 Exception-free filters} *)

(**
   The following functions operate on lists of JSON nodes.
   None of them raises an exception when a certain kind of node is expected
   but no node or the wrong kind of node is found.
   Instead of raising an exception, nodes that are not as expected
   are simply ignored.
*)

val filter_map : ('a -> 'b option) -> 'a list -> 'b list
  (** [filter_map f l] maps each element of the list [l] to an optional value
      using function [f] and unwraps the resulting values. *)

val flatten : t list -> t list
  (** Expects JSON arrays and returns all their elements as a single
      list. [flatten l] is equivalent to [List.flatten (filter_list l)]. *)

val filter_index : int -> t list -> t list
  (** Expects JSON arrays and returns all their elements existing at the given
      position. *)

val filter_list : t list -> t list list
  (** Expects JSON arrays and unwraps them. *)

val filter_member : string -> t list -> t list
  (** Expects JSON objects and returns all the fields of the given name
      (at most one field per object). *)

val filter_assoc : t list -> (string * t) list list
  (** Expects JSON objects and unwraps them. *)

val filter_bool : t list -> bool list
  (** Expects JSON booleans and unwraps them. *)

val filter_int : t list -> int list
  (** Expects JSON integers ([`Int] nodes) and unwraps them. *)

val filter_float : t list -> float list
  (** Expects JSON floats ([`Float] nodes) and unwraps them. *)

val filter_number : t list -> float list
  (** Expects JSON numbers ([`Int] or [`Float]) and unwraps them.
      Ints are converted to floats. *)

val filter_string : t list -> string list
  (** Expects JSON strings and unwraps them. *)
# 51 "yojson.cppo.mli"
end
# 55 "yojson.cppo.mli"
end

(** {1 Multipurpose JSON tree type} *)

module Safe :
sig
(**
   This module supports a specific syntax for variants and tuples
   in addition to the standard JSON nodes.
   Arbitrary integers are supported and represented as a decimal string 
   using [`Intlit] when they cannot be represented using OCaml's int type.

   This module is recommended for intensive use 
   or OCaml-friendly use of JSON.
*)

# 1 "type.ml"
(** {3 Type of the JSON tree} *)

type t =
    [
    | `Null
    | `Bool of bool
    
# 8 "type.ml"
    | `Int of int
    
# 11 "type.ml"
    | `Intlit of string
    
# 14 "type.ml"
    | `Float of float
    
# 20 "type.ml"
    | `String of string
    
# 25 "type.ml"
    | `Assoc of (string * t) list
    | `List of t list
    
# 28 "type.ml"
    | `Tuple of t list
    
# 31 "type.ml"
    | `Variant of (string * t option)
    
# 33 "type.ml"
    ]
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
# 1 "monomorphic.mli"
val pp : Format.formatter -> t -> unit
  (** Pretty printer, useful for debugging *)

val show : t -> string
  (** Convert value to string, useful for debugging *)

val equal : t -> t -> bool
  (** [equal a b] is the monomorphic equality.
      Determines whether two JSON values are considered equal. In the case of
      JSON objects, the order of the keys does not matter, except for
      duplicate keys which will be considered equal as long as they are in the
      same input order.
    *)
# 1 "safe.mli"
val to_basic : t -> Basic.t
  (**
     Tuples are converted to JSON arrays,
     Variants are converted to JSON strings or arrays of a string (constructor)
     and a json value (argument).
     Long integers are converted to JSON strings.

     Examples:
{v
`Tuple [ `Int 1; `Float 2.3 ]   ->    `List [ `Int 1; `Float 2.3 ]
`Variant ("A", None)            ->    `String "A"
`Variant ("B", Some x)          ->    `List [ `String "B", x ]
`Intlit "12345678901234567890"  ->    `String "12345678901234567890"
v}
  *)
# 1 "write.mli"
(** {2 JSON writers} *)

val to_string :
  ?buf:Bi_outbuf.t ->
  ?len:int ->
  ?std:bool ->
  t -> string
  (** Write a compact JSON value to a string.
      @param buf allows to reuse an existing buffer created with
      [Bi_outbuf.create]. The buffer is cleared of all contents
      before starting and right before returning.
      @param len initial length of the output buffer.
      @param std use only standard JSON syntax,
      i.e. convert tuples and variants into standard JSON (if applicable),
      refuse to print NaN and infinities,
      require the root node to be either an object or an array.
      Default is [false].
  *)

val to_channel :
  ?buf:Bi_outbuf.t ->
  ?len:int ->
  ?std:bool ->
  out_channel -> t -> unit
  (** Write a compact JSON value to a channel.
      @param buf allows to reuse an existing buffer created with
      [Bi_outbuf.create_channel_writer] on the same channel.
      [buf] is flushed right
      before [to_channel] returns but the [out_channel] is
      not flushed automatically.

      See [to_string] for the role of the other optional arguments. *)

val to_output :
  ?buf:Bi_outbuf.t ->
  ?len:int ->
  ?std:bool ->
  < output : string -> int -> int -> int; .. > -> t -> unit
  (** Write a compact JSON value to an OO channel.
      @param buf allows to reuse an existing buffer created with
      [Bi_outbuf.create_output_writer] on the same channel.
      [buf] is flushed right
      before [to_output] returns but the channel itself is
      not flushed automatically.

      See [to_string] for the role of the other optional arguments. *)

val to_file :
  ?len:int ->
  ?std:bool ->
  string -> t -> unit
  (** Write a compact JSON value to a file.
      See [to_string] for the role of the optional arguments. *)

val to_outbuf :
  ?std:bool ->
  Bi_outbuf.t -> t -> unit
  (** Write a compact JSON value to an existing buffer.
      See [to_string] for the role of the optional argument. *)

val stream_to_string :
  ?buf:Bi_outbuf.t ->
  ?len:int ->
  ?std:bool ->
  t Stream.t -> string
  (** Write a newline-separated sequence of compact one-line JSON values to
      a string.
      See [to_string] for the role of the optional arguments. *)

val stream_to_channel :
  ?buf:Bi_outbuf.t ->
  ?len:int ->
  ?std:bool ->
  out_channel -> t Stream.t -> unit
  (** Write a newline-separated sequence of compact one-line JSON values to
      a channel.
      See [to_channel] for the role of the optional arguments. *)

val stream_to_file :
  ?len:int ->
  ?std:bool ->
  string -> t Stream.t -> unit
  (** Write a newline-separated sequence of compact one-line JSON values to
      a file.
      See [to_string] for the role of the optional arguments. *)

val stream_to_outbuf :
  ?std:bool ->
  Bi_outbuf.t ->
  t Stream.t -> unit
  (** Write a newline-separated sequence of compact one-line JSON values to
      an existing buffer.
      See [to_string] for the role of the optional arguments. *)

val write_t : Bi_outbuf.t -> t -> unit
(** Write the given JSON value to the given buffer.
    Provided as a writer function for atdgen.
*)

(** {2 Miscellaneous} *)

val sort : t -> t
  (** Sort object fields (stable sort, comparing field names
      and treating them as byte sequences) *)
# 1 "write2.mli"
(** {2 JSON pretty-printing} *)

val pretty_format : ?std:bool -> t -> Easy_format.t
  (** Convert into a pretty-printable tree.
      See [to_string] for the role of the optional [std] argument.

      @see <http://martin.jambon.free.fr/easy-format.html> Easy-format
  *)

val pretty_print : ?std:bool -> Format.formatter -> t -> unit
  (** Pretty-print into a {!Format.formatter}.
      See [to_string] for the role of the optional [std] argument.

      @since 1.3.1 *)

val pretty_to_string : ?std:bool -> t -> string
  (** Pretty-print into a string.
      See [to_string] for the role of the optional [std] argument.
  *)

val pretty_to_channel : ?std:bool -> out_channel -> t -> unit
  (** Pretty-print to a channel.
      See [to_string] for the role of the optional [std] argument.
  *)
# 1 "read.mli"
val prettify : ?std:bool -> string -> string
  (** Combined parser and pretty-printer.
      See [to_string] for the role of the optional [std] argument. *)

val compact : ?std:bool -> string -> string
  (** Combined parser and printer.
      See [to_string] for the role of the optional [std] argument. *)

(** {2 JSON readers} *)

exception Finally of exn * exn
(** Exception describing a failure in both finalizer and parsing. *)

val from_string :
  ?buf:Bi_outbuf.t ->
  ?fname:string ->
  ?lnum:int ->
  string -> t
  (** Read a JSON value from a string.
      @param buf use this buffer at will during parsing instead of creating
      a new one.
      @param fname data file name to be used in error messages. It does
      not have to be a real file.
      @param lnum number of the first line of input. Default is 1.
  *)

val from_channel :
  ?buf:Bi_outbuf.t ->
  ?fname:string ->
  ?lnum:int ->
  in_channel -> t
  (** Read a JSON value from a channel.
      See [from_string] for the meaning of the optional arguments. *)

val from_file :
  ?buf:Bi_outbuf.t ->
  ?fname:string ->
  ?lnum:int ->
  string -> t
  (** Read a JSON value from a file.
      See [from_string] for the meaning of the optional arguments. *)



type json_line = [ `Json of t | `Exn of exn ]
    (** The type of values resulting from a parsing attempt of a JSON value. *)

val linestream_from_channel :
  ?buf:Bi_outbuf.t ->
  ?fin:(unit -> unit) ->
  ?fname:string ->
  ?lnum:int ->
  in_channel -> json_line Stream.t
  (** Input a sequence of JSON values, one per line, from a channel.
      Exceptions raised when reading malformed lines are caught
      and represented using [`Exn].

      See [stream_from_channel] for the meaning of the optional [fin]
      argument.
      See [from_string] for the meaning of the other optional arguments. *)

val linestream_from_file :
  ?buf:Bi_outbuf.t ->
  ?fname:string ->
  ?lnum:int ->
  string -> json_line Stream.t
  (** Input a sequence of JSON values, one per line, from a file.
      Exceptions raised when reading malformed lines are caught
      and represented using [`Exn].

      See [stream_from_channel] for the meaning of the optional [fin]
      argument.
      See [from_string] for the meaning of the other optional arguments. *)


# 83 "yojson.cppo.mli"
module Util :
sig
# 1 "util.mli"
(**
   This module provides combinators for extracting fields from JSON
   values. This approach is recommended for reading a few fields
   from data returned by public APIs. However for more complex applications
   we recommend {{:https://github.com/MyLifeLabs/atdgen}Atdgen}.

   Here is some sample JSON data:
{v
\{
  "id": "398eb027",
  "name": "John Doe",
  "pages": [
    \{
      "id": 1,
      "title": "The Art of Flipping Coins",
      "url": "http://example.com/398eb027/1"
    },
    \{
      "id": 2,
      "deleted": true
    },
    \{
      "id": 3,
      "title": "Artichoke Salad",
      "url": "http://example.com/398eb027/3"
    },
    \{
      "id": 4,
      "title": "Flying Bananas",
      "url": "http://example.com/398eb027/4"
    }
  ]
}
v}

   In order to extract the "id" field, assuming it is mandatory,
   we would use the following OCaml code that operates on single JSON
   nodes:
{v
open Yojson.Basic.Util
...
  let id = json |> member "id" |> to_string in
  ...
v}

   In order to extract all the "title" fields, we would write the following
   OCaml code that operates on lists of JSON nodes, skipping
   undefined nodes and nodes of unexpected type:
{v
open Yojson.Basic.Util

let extract_titles (json : Yojson.Basic.t) : string list =
  [json]
    |> filter_member "pages"
    |> flatten
    |> filter_member "title"
    |> filter_string
v}
*)

exception Type_error of string * t
  (** Raised when the JSON value is not of the correct type to support an
      operation, e.g. [member] on an [`Int]. The string message explains the
      mismatch. *)

exception Undefined of string * t
  (** Raised when the equivalent JavaScript operation on the JSON value would
      return undefined. Currently this only happens when an array index is out
      of bounds. *)

val ( |> ) : 'a -> ('a -> 'b) -> 'b
(** @deprecated Forward pipe operator; useful for composing JSON
    access functions without too many parentheses *)

val keys : t -> string list
  (** Returns all the key names in the given JSON object *)

val values : t -> t list
  (** Return all the value in the given JSON object *)

val combine : t -> t -> t
  (** Combine two JSON Objects together *)

val member : string -> t -> t
  (** [member k obj] returns the value associated with the key [k] in the JSON
      object [obj], or [`Null] if [k] is not present in [obj]. *)

val index : int -> t -> t
  (** [index i arr] returns the value at index [i] in the JSON array [arr].
      Negative indices count from the end of the list (so -1 is the last
      element). *)

val map : (t -> t) -> t -> t
  (** [map f arr] calls the function [f] on each element of the JSON array
      [arr], and returns a JSON array containing the results. *)

val to_assoc : t -> (string * t) list
  (** Extract the items of a JSON object or raise [Type_error]. *)

val to_option : (t -> 'a) -> t -> 'a option
  (** Return [None] if the JSON value is null or map the JSON value
      to [Some] value using the provided function. *)

val to_bool : t -> bool
  (** Extract a boolean value or raise [Type_error]. *)

val to_bool_option : t -> bool option
  (** Extract [Some] boolean value,
      return [None] if the value is null,
      or raise [Type_error] otherwise. *)

val to_number : t -> float
  (** Extract a number or raise [Type_error]. *)

val to_number_option : t -> float option
  (** Extract [Some] number,
      return [None] if the value is null,
      or raise [Type_error] otherwise. *)

val to_float : t -> float
  (** Extract a float value or raise [Type_error].
      [to_number] is generally preferred as it also works with int literals. *)

val to_float_option : t -> float option
  (** Extract [Some] float value,
      return [None] if the value is null,
      or raise [Type_error] otherwise.
      [to_number_option] is generally preferred as it also works
      with int literals. *)

val to_int : t -> int
  (** Extract an int from a JSON int or raise [Type_error]. *)

val to_int_option : t -> int option
  (** Extract [Some] int from a JSON int,
      return [None] if the value is null,
      or raise [Type_error] otherwise. *)

val to_list : t -> t list
  (** Extract a list from JSON array or raise [Type_error]. *)

val to_string : t -> string
  (** Extract a string from a JSON string or raise [Type_error]. *)

val to_string_option : t -> string option
  (** Extract [Some] string from a JSON string,
      return [None] if the value is null,
      or raise [Type_error] otherwise. *)

val convert_each : (t -> 'a) -> t -> 'a list
  (** The conversion functions above cannot be used with [map], because they do
      not return JSON values. This convenience function [convert_each to_f arr]
      is equivalent to [List.map to_f (to_list arr)]. *)


(** {3 Exception-free filters} *)

(**
   The following functions operate on lists of JSON nodes.
   None of them raises an exception when a certain kind of node is expected
   but no node or the wrong kind of node is found.
   Instead of raising an exception, nodes that are not as expected
   are simply ignored.
*)

val filter_map : ('a -> 'b option) -> 'a list -> 'b list
  (** [filter_map f l] maps each element of the list [l] to an optional value
      using function [f] and unwraps the resulting values. *)

val flatten : t list -> t list
  (** Expects JSON arrays and returns all their elements as a single
      list. [flatten l] is equivalent to [List.flatten (filter_list l)]. *)

val filter_index : int -> t list -> t list
  (** Expects JSON arrays and returns all their elements existing at the given
      position. *)

val filter_list : t list -> t list list
  (** Expects JSON arrays and unwraps them. *)

val filter_member : string -> t list -> t list
  (** Expects JSON objects and returns all the fields of the given name
      (at most one field per object). *)

val filter_assoc : t list -> (string * t) list list
  (** Expects JSON objects and unwraps them. *)

val filter_bool : t list -> bool list
  (** Expects JSON booleans and unwraps them. *)

val filter_int : t list -> int list
  (** Expects JSON integers ([`Int] nodes) and unwraps them. *)

val filter_float : t list -> float list
  (** Expects JSON floats ([`Float] nodes) and unwraps them. *)

val filter_number : t list -> float list
  (** Expects JSON numbers ([`Int] or [`Float]) and unwraps them.
      Ints are converted to floats. *)

val filter_string : t list -> string list
  (** Expects JSON strings and unwraps them. *)
# 86 "yojson.cppo.mli"
end
# 93 "yojson.cppo.mli"
end

(** {1 JSON tree type with literal int/float/string leaves} *)

module Raw :
sig
(**
   Ints, floats and strings literals are systematically preserved using
   [`Intlit], [`Floatlit] and [`Stringlit].
   This module also supports the specific syntax for variants and tuples
   supported by {!Yojson.Safe}.
*)

# 1 "type.ml"
(** {3 Type of the JSON tree} *)

type t =
    [
    | `Null
    | `Bool of bool
    
# 11 "type.ml"
    | `Intlit of string
    
# 17 "type.ml"
    | `Floatlit of string
    
# 23 "type.ml"
    | `Stringlit of string
    
# 25 "type.ml"
    | `Assoc of (string * t) list
    | `List of t list
    
# 28 "type.ml"
    | `Tuple of t list
    
# 31 "type.ml"
    | `Variant of (string * t option)
    
# 33 "type.ml"
    ]
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
# 1 "monomorphic.mli"
val pp : Format.formatter -> t -> unit
  (** Pretty printer, useful for debugging *)

val show : t -> string
  (** Convert value to string, useful for debugging *)

val equal : t -> t -> bool
  (** [equal a b] is the monomorphic equality.
      Determines whether two JSON values are considered equal. In the case of
      JSON objects, the order of the keys does not matter, except for
      duplicate keys which will be considered equal as long as they are in the
      same input order.
    *)
# 1 "write.mli"
(** {2 JSON writers} *)

val to_string :
  ?buf:Bi_outbuf.t ->
  ?len:int ->
  ?std:bool ->
  t -> string
  (** Write a compact JSON value to a string.
      @param buf allows to reuse an existing buffer created with
      [Bi_outbuf.create]. The buffer is cleared of all contents
      before starting and right before returning.
      @param len initial length of the output buffer.
      @param std use only standard JSON syntax,
      i.e. convert tuples and variants into standard JSON (if applicable),
      refuse to print NaN and infinities,
      require the root node to be either an object or an array.
      Default is [false].
  *)

val to_channel :
  ?buf:Bi_outbuf.t ->
  ?len:int ->
  ?std:bool ->
  out_channel -> t -> unit
  (** Write a compact JSON value to a channel.
      @param buf allows to reuse an existing buffer created with
      [Bi_outbuf.create_channel_writer] on the same channel.
      [buf] is flushed right
      before [to_channel] returns but the [out_channel] is
      not flushed automatically.

      See [to_string] for the role of the other optional arguments. *)

val to_output :
  ?buf:Bi_outbuf.t ->
  ?len:int ->
  ?std:bool ->
  < output : string -> int -> int -> int; .. > -> t -> unit
  (** Write a compact JSON value to an OO channel.
      @param buf allows to reuse an existing buffer created with
      [Bi_outbuf.create_output_writer] on the same channel.
      [buf] is flushed right
      before [to_output] returns but the channel itself is
      not flushed automatically.

      See [to_string] for the role of the other optional arguments. *)

val to_file :
  ?len:int ->
  ?std:bool ->
  string -> t -> unit
  (** Write a compact JSON value to a file.
      See [to_string] for the role of the optional arguments. *)

val to_outbuf :
  ?std:bool ->
  Bi_outbuf.t -> t -> unit
  (** Write a compact JSON value to an existing buffer.
      See [to_string] for the role of the optional argument. *)

val stream_to_string :
  ?buf:Bi_outbuf.t ->
  ?len:int ->
  ?std:bool ->
  t Stream.t -> string
  (** Write a newline-separated sequence of compact one-line JSON values to
      a string.
      See [to_string] for the role of the optional arguments. *)

val stream_to_channel :
  ?buf:Bi_outbuf.t ->
  ?len:int ->
  ?std:bool ->
  out_channel -> t Stream.t -> unit
  (** Write a newline-separated sequence of compact one-line JSON values to
      a channel.
      See [to_channel] for the role of the optional arguments. *)

val stream_to_file :
  ?len:int ->
  ?std:bool ->
  string -> t Stream.t -> unit
  (** Write a newline-separated sequence of compact one-line JSON values to
      a file.
      See [to_string] for the role of the optional arguments. *)

val stream_to_outbuf :
  ?std:bool ->
  Bi_outbuf.t ->
  t Stream.t -> unit
  (** Write a newline-separated sequence of compact one-line JSON values to
      an existing buffer.
      See [to_string] for the role of the optional arguments. *)

val write_t : Bi_outbuf.t -> t -> unit
(** Write the given JSON value to the given buffer.
    Provided as a writer function for atdgen.
*)

(** {2 Miscellaneous} *)

val sort : t -> t
  (** Sort object fields (stable sort, comparing field names
      and treating them as byte sequences) *)


# 1 "write2.mli"
(** {2 JSON pretty-printing} *)

val pretty_format : ?std:bool -> t -> Easy_format.t
  (** Convert into a pretty-printable tree.
      See [to_string] for the role of the optional [std] argument.

      @see <http://martin.jambon.free.fr/easy-format.html> Easy-format
  *)

val pretty_print : ?std:bool -> Format.formatter -> t -> unit
  (** Pretty-print into a {!Format.formatter}.
      See [to_string] for the role of the optional [std] argument.

      @since 1.3.1 *)

val pretty_to_string : ?std:bool -> t -> string
  (** Pretty-print into a string.
      See [to_string] for the role of the optional [std] argument.
  *)

val pretty_to_channel : ?std:bool -> out_channel -> t -> unit
  (** Pretty-print to a channel.
      See [to_string] for the role of the optional [std] argument.
  *)
# 1 "read.mli"
val prettify : ?std:bool -> string -> string
  (** Combined parser and pretty-printer.
      See [to_string] for the role of the optional [std] argument. *)

val compact : ?std:bool -> string -> string
  (** Combined parser and printer.
      See [to_string] for the role of the optional [std] argument. *)

(** {2 JSON readers} *)

exception Finally of exn * exn
(** Exception describing a failure in both finalizer and parsing. *)

val from_string :
  ?buf:Bi_outbuf.t ->
  ?fname:string ->
  ?lnum:int ->
  string -> t
  (** Read a JSON value from a string.
      @param buf use this buffer at will during parsing instead of creating
      a new one.
      @param fname data file name to be used in error messages. It does
      not have to be a real file.
      @param lnum number of the first line of input. Default is 1.
  *)

val from_channel :
  ?buf:Bi_outbuf.t ->
  ?fname:string ->
  ?lnum:int ->
  in_channel -> t
  (** Read a JSON value from a channel.
      See [from_string] for the meaning of the optional arguments. *)

val from_file :
  ?buf:Bi_outbuf.t ->
  ?fname:string ->
  ?lnum:int ->
  string -> t
  (** Read a JSON value from a file.
      See [from_string] for the meaning of the optional arguments. *)

type json_line = [ `Json of t | `Exn of exn ]
    (** The type of values resulting from a parsing attempt of a JSON value. *)

val linestream_from_channel :
  ?buf:Bi_outbuf.t ->
  ?fin:(unit -> unit) ->
  ?fname:string ->
  ?lnum:int ->
  in_channel -> json_line Stream.t
  (** Input a sequence of JSON values, one per line, from a channel.
      Exceptions raised when reading malformed lines are caught
      and represented using [`Exn].

      See [stream_from_channel] for the meaning of the optional [fin]
      argument.
      See [from_string] for the meaning of the other optional arguments. *)

val linestream_from_file :
  ?buf:Bi_outbuf.t ->
  ?fname:string ->
  ?lnum:int ->
  string -> json_line Stream.t
  (** Input a sequence of JSON values, one per line, from a file.
      Exceptions raised when reading malformed lines are caught
      and represented using [`Exn].

      See [stream_from_channel] for the meaning of the optional [fin]
      argument.
      See [from_string] for the meaning of the other optional arguments. *)
# 121 "yojson.cppo.mli"
end

(** {1 Supertype of all JSON tree types} *)

# 1 "type.ml"
(** {3 Type of the JSON tree} *)

type t =
    [
    | `Null
    | `Bool of bool
    
# 8 "type.ml"
    | `Int of int
    
# 11 "type.ml"
    | `Intlit of string
    
# 14 "type.ml"
    | `Float of float
    
# 17 "type.ml"
    | `Floatlit of string
    
# 20 "type.ml"
    | `String of string
    
# 23 "type.ml"
    | `Stringlit of string
    
# 25 "type.ml"
    | `Assoc of (string * t) list
    | `List of t list
    
# 28 "type.ml"
    | `Tuple of t list
    
# 31 "type.ml"
    | `Variant of (string * t option)
    
# 33 "type.ml"
    ]
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
# 1 "monomorphic.mli"
val pp : Format.formatter -> t -> unit
  (** Pretty printer, useful for debugging *)

val show : t -> string
  (** Convert value to string, useful for debugging *)

val equal : t -> t -> bool
  (** [equal a b] is the monomorphic equality.
      Determines whether two JSON values are considered equal. In the case of
      JSON objects, the order of the keys does not matter, except for
      duplicate keys which will be considered equal as long as they are in the
      same input order.
    *)
# 135 "yojson.cppo.mli"
type json_max = t
# 1 "write.mli"
(** {2 JSON writers} *)

val to_string :
  ?buf:Bi_outbuf.t ->
  ?len:int ->
  ?std:bool ->
  t -> string
  (** Write a compact JSON value to a string.
      @param buf allows to reuse an existing buffer created with
      [Bi_outbuf.create]. The buffer is cleared of all contents
      before starting and right before returning.
      @param len initial length of the output buffer.
      @param std use only standard JSON syntax,
      i.e. convert tuples and variants into standard JSON (if applicable),
      refuse to print NaN and infinities,
      require the root node to be either an object or an array.
      Default is [false].
  *)

val to_channel :
  ?buf:Bi_outbuf.t ->
  ?len:int ->
  ?std:bool ->
  out_channel -> t -> unit
  (** Write a compact JSON value to a channel.
      @param buf allows to reuse an existing buffer created with
      [Bi_outbuf.create_channel_writer] on the same channel.
      [buf] is flushed right
      before [to_channel] returns but the [out_channel] is
      not flushed automatically.

      See [to_string] for the role of the other optional arguments. *)

val to_output :
  ?buf:Bi_outbuf.t ->
  ?len:int ->
  ?std:bool ->
  < output : string -> int -> int -> int; .. > -> t -> unit
  (** Write a compact JSON value to an OO channel.
      @param buf allows to reuse an existing buffer created with
      [Bi_outbuf.create_output_writer] on the same channel.
      [buf] is flushed right
      before [to_output] returns but the channel itself is
      not flushed automatically.

      See [to_string] for the role of the other optional arguments. *)

val to_file :
  ?len:int ->
  ?std:bool ->
  string -> t -> unit
  (** Write a compact JSON value to a file.
      See [to_string] for the role of the optional arguments. *)

val to_outbuf :
  ?std:bool ->
  Bi_outbuf.t -> t -> unit
  (** Write a compact JSON value to an existing buffer.
      See [to_string] for the role of the optional argument. *)

val stream_to_string :
  ?buf:Bi_outbuf.t ->
  ?len:int ->
  ?std:bool ->
  t Stream.t -> string
  (** Write a newline-separated sequence of compact one-line JSON values to
      a string.
      See [to_string] for the role of the optional arguments. *)

val stream_to_channel :
  ?buf:Bi_outbuf.t ->
  ?len:int ->
  ?std:bool ->
  out_channel -> t Stream.t -> unit
  (** Write a newline-separated sequence of compact one-line JSON values to
      a channel.
      See [to_channel] for the role of the optional arguments. *)

val stream_to_file :
  ?len:int ->
  ?std:bool ->
  string -> t Stream.t -> unit
  (** Write a newline-separated sequence of compact one-line JSON values to
      a file.
      See [to_string] for the role of the optional arguments. *)

val stream_to_outbuf :
  ?std:bool ->
  Bi_outbuf.t ->
  t Stream.t -> unit
  (** Write a newline-separated sequence of compact one-line JSON values to
      an existing buffer.
      See [to_string] for the role of the optional arguments. *)

val write_t : Bi_outbuf.t -> t -> unit
(** Write the given JSON value to the given buffer.
    Provided as a writer function for atdgen.
*)

(** {2 Miscellaneous} *)

val sort : t -> t
  (** Sort object fields (stable sort, comparing field names
      and treating them as byte sequences) *)

# 1 "write2.mli"
(** {2 JSON pretty-printing} *)

val pretty_format : ?std:bool -> t -> Easy_format.t
  (** Convert into a pretty-printable tree.
      See [to_string] for the role of the optional [std] argument.

      @see <http://martin.jambon.free.fr/easy-format.html> Easy-format
  *)

val pretty_print : ?std:bool -> Format.formatter -> t -> unit
  (** Pretty-print into a {!Format.formatter}.
      See [to_string] for the role of the optional [std] argument.

      @since 1.3.1 *)

val pretty_to_string : ?std:bool -> t -> string
  (** Pretty-print into a string.
      See [to_string] for the role of the optional [std] argument.
  *)

val pretty_to_channel : ?std:bool -> out_channel -> t -> unit
  (** Pretty-print to a channel.
      See [to_string] for the role of the optional [std] argument.
  *)

