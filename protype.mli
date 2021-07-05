(** Type descriptions from which one can derive encodings for protocols. *)

module Id:
sig
  (** Identifiers for record fields, enum values and variant constructors. *)

  (** Identifiers.

      Internally, each identifier is a name (a string) associated to a tag (an integer).
      There is a bijection between names and tags. When encoding values to binary,
      we use the tags to avoid wasting space. But tags depend on the order in which
      identifiers are declared. To avoid having to specify the tag manually (which is
      quite error-prone), toplevel value encodings start with a table of all tags and
      their associated names. Decoders use this table to translate tags from a specific
      encoded value to the tags that are actually declared in the decoder. *)
  type t

  (** Make an identifier from a name.

      If you already called [make] on this name before, this will return an identifier
      with the same tag. Otherwise, this name will be given a new unique tag. *)
  val make: string -> t

  (** Get the name of an identifier. *)
  val name: t -> string

  (** Get the tag of an identifier.

      Usually there is no need to use this except for debugging purposes. *)
  val tag: t -> int

  (** Compare two identifiers.

      This compares tags, so [make name] will always be considered to be equal
      to another [make name] (with the same [name]). *)
  val compare: t -> t -> int
end

(** Short-hand for [Id.make]. *)
val id: string -> Id.t

module Id_set: Set.S with type elt = Id.t
module Id_map: Map.S with type key = Id.t

(** Version numbers.

    Toplevel values are encoded with a version number.
    Type descriptions can be set to depend on this number.
    This allows type descriptions to support decoding older versions. *)
type version = int

module Version_map: Map.S with type key = version

(** Type descriptions.

    This type is provided in case you want to build your own encoders and decoders.
    You can build values of type [t] directly, but it may be more convenient to use
    the constructor functions below (which also document each case). *)
type _ t =
  | Unit: unit t
  | Bool: bool t
  | Char: char t
  | Int: int t
  | Int32: int32 t
  | Int64: int64 t
  | Float: float t
  | String: string t
  | Option: 'a t -> 'a option t
  | Array: 'a t -> 'a array t
  | List: 'a t -> 'a list t
  | Record: {
      fields: ('r, 'f) fields;
      make: 'f;
      can_ignore: Id_set.t;
      rename: Id.t Id_map.t;
    } -> 'r t
  | Enum: {
      cases: 'a Id_map.t;
      id: 'a -> Id.t;
      rename: Id.t Id_map.t;
    } -> 'a t
  | Variant: {
      cases: 'a packed_case Id_map.t;
      get: 'a -> 'a packed_case_value;
      rename: Id.t Id_map.t;
    } -> 'a t
  | Convert: {
      typ: 'a t;
      encode: 'b -> 'a;
      decode: 'a -> 'b option;
    } -> 'b t
  | Versions: {
      old: 'a t;
      from: version;
      current: 'a t;
    } -> 'a t

(** Field descriptions for [Record] types. *)
and (_, _) fields =
  | R_field: {
      id: Id.t;
      typ: 'a t;
      get: 'r -> 'a;
      default: 'a option;
      next: ('r, 'b) fields
    } -> ('r, 'a -> 'b) fields
  | R_last: {
      id: Id.t;
      typ: 'a t;
      get: 'r -> 'a;
      default: 'a option;
    } -> ('r, 'a -> 'r) fields

(** Case descriptions for [Variant] types. *)
and ('v, 'a) case =
  {
    id: Id.t;
    typ: 'a t;
    make: 'a -> 'v;
  }

(** Case descriptions for [Variant] types (existentially-packed version). *)
and 'v packed_case = Case: ('v, _) case -> 'v packed_case

(** Case descriptions for [Variant] types (existentially-packed version). *)
and 'v packed_case_value = Value: ('v, 'a) case * 'a -> 'v packed_case_value

(** Existentially hidden type descriptions. *)
type e = E: _ t -> e

(** {2 Base Types} *)

(** Type description for [unit]. *)
val unit: unit t

(** Type description for [bool]. *)
val bool: bool t

(** Type description for [char]. *)
val char: char t

(** Type description for [int32]. *)
val int32: int32 t

(** Type description for [int64]. *)
val int64: int64 t

(** Type description for [int]. *)
val int: int t

(** Type description for [float]. *)
val float: float t

(** Type description for [string]. *)
val string: string t

(** {2 Simple Parameterized Types} *)

(** Make a type description for an [option] type. *)
val option: 'a t -> 'a option t

(** Make a type description for an [array] type. *)
val array: 'a t -> 'a array t

(** Make a type description for an [list] type. *)
val list: 'a t -> 'a list t

(** {2 Record Types} *)

(** Partial record definitions.

    A value of type [('r, 'p, 'f) definition] defines parts of the fields
    for a record type ['r].

    Type ['f] is the type of the function that creates values of type ['r]
    (the record maker). It is a function that takes the value of all fields
    and returns a new record.

    Type ['p] is a partial version of ['f]. It is ['f] but without the fields
    that have not yet been defined. *)
type ('r, 'p, 'f) partial_record = ('r, 'p) fields * 'f

(** Finalize the definition of a record.

    Here is a full example of a record definition:
    {[
      type abc = { a: int32; b: bool; c: string }

      let abc =
        let open Protype in
        record @@
        ("a", int32, fun r -> r.a) @
        ("b", bool, false, fun r -> r.b) @?
        ("c", string, fun r -> r.c) @:
        fun a b c -> { a; b; c }
    ]}
    Fields [a] and [c] are mandatory but field [b] has a default value of [false].

    Note how the [partial_record] requires both functions types to be equal (to ['f]):
    it means that all fields have been defined.

    [can_ignore] is a list of fields that can be ignored safely if they are seen
    in an encoded value. They are fields that were used in previous versions
    of the encoding but that are no longer used and whose value can be dropped safely.

    [rename] is a list of field renamings [(before, after)]. If a field with name
    [before] is seen while decoding a value, it should be understood as actually
    being the [after] field. This allows to rename fields safely and still be
    backward-compatible. *)
val record:
  ?can_ignore: string list ->
  ?rename: (string * string) list ->
  ('r, 'f, 'f) partial_record -> 'r t

(** Define a record field.

    Usage: [(id, typ, get) @ next]

    This defines a field named [id], of type [typ] and with getter [get].
    The getter is a function that takes a record and returns the field.

    The record definition continues in [next]. *)
val (@):
  string * 'a t * ('r -> 'a) ->
  ('r, 'p, 'f) partial_record ->
  ('r, 'a -> 'p, 'f) partial_record

(** Define a record field with a default value.

    Usage: [(id, typ, default, get) @? next]

    This is the same as [(id, typ, get) @ next] except that the field has default
    value [default]. This allows the field to be absent from an encoded value. *)
val (@?):
  string * 'a t * 'a * ('r -> 'a) ->
  ('r, 'p, 'f) partial_record ->
  ('r, 'a -> 'p, 'f) partial_record

(** Define the last record field and the record maker.

    Usage: [(id, typ, get) @: f]

    This is similar to [(id, typ, get) @ f] except that there are no further
    field to define. Function [f] is the record maker. *)
val (@:):
  string * 'a t * ('r -> 'a) ->
  'f ->
  ('r, 'a -> 'r, 'f) partial_record

(** Define the last record field, with a default value, and the record maker.

    Usage: [(id, typ, default, get) @?: f]

    This is the same as [(id, typ, get) @: f] except that the field has default
    value [default]. *)
val (@?:):
  string * 'a t * 'a * ('r -> 'a) ->
  'f ->
  ('r, 'a -> 'r, 'f) partial_record

(** {2 Sum Types} *)

(** Make a type description for an enum type.

    Enum types are sum types where values are simple constants with no parameters.

    Here is a full example of an enum definition:
    {[
      type abc = A | B | C

      let abc =
        let open Protype in
        let a = id "A" in
        let b = id "B" in
        let c = id "C" in
        enum [ a, A; b, B; c, C ] @@ function A -> a | B -> b | C -> c
    ]}

    Make sure not to forget record cases. The type-checker will make sure you do not
    forget cases in the pattern-matching of the function, but it will not help
    you with in list of cases.

    Note that enums can also be encoded using [convert int32], but:
    - [convert] expect the encoding function to be full, not partial;
    - giving names makes it easier to reverse engineer encoded values.

    [rename] is a list of renamings [(before, after)]. If a value with name
    [before] is seen while decoding, it should be understood as actually
    being the [after] value. This allows to rename values and still be
    backward-compatible. *)
val enum: ?rename: (Id.t * Id.t) list -> (Id.t * 'a) list -> ('a -> Id.t) -> 'a t

(** Make a case description for a variant type.

    Usage: [case name typ make]

    This defines a case with constructor name [name], parameter type [typ]
    and maker [make]. The maker is a function that builds a value for this
    constructor from a parameter.

    See example in the documentation of [variant]. *)
val case: string -> 'a t -> ('a -> 'v) -> ('v, 'a) case

(** Make a case description for a variant with no parameter.

    Usage: [case name value]

    This is equivalent to [case name unit (fun () -> value)]. *)
val case_unit: string -> 'v -> ('v, unit) case

(** Make a packed case value.

    Usage: [value case parameter]

    This packs variant case [case] and the parameter of the constructor [parameter]
    into a packed case value where the type of the parameter is existentially hidden.

    Those values are used as the result of the deconstruction function
    given to [variant]. See example in the documentation of [variant]. *)
val value: ('v, 'a) case -> 'a -> 'v packed_case_value

(** Make a type description for a variant type.

    Variant types are sum types where values can have parameters.

    Here is a full example of a variant definition:
    {[
      type abc = A | B of bool | C of int32

      let abc =
        let open Protype in
        let a = case "A" unit (fun () -> A) in
        let b = case "B" bool (fun x -> B x) in
        let c = case "C" int32 (fun x -> C x) in
        variant [ Case a; Case b; Case c ] @@
        function A -> value a () | B x -> value b x | C x -> value c x
    ]}

    [rename] is a list of renamings [(before, after)]. If a variant with
    constructor name [before] is seen while decoding, it should be understood
    as actually being the [after] constructor. This allows to rename constructors
    and still be backward-compatible. *)
val variant: ?rename: (string * string) list -> 'a packed_case list ->
  ('a -> 'a packed_case_value) -> 'a t

(** {2 Conversions} *)

(** Make a type description by converting another type description.

    Example: [convert_partial string ~encode: Int32.to_string ~decode: Int32.of_string_opt]

    This example encodes integers as strings.
    To encode, it calls [Int32.to_string] and then use string encoding.
    To decode, it use string decoding and then calls [Int32.of_string_opt].

    [decode] can be partial: it can return [None] for invalid encodings.
    [encode] is not partial though: all values of the resulting type description
    must be encodable. *)
val convert_partial: 'a t -> encode: ('b -> 'a) -> decode: ('a -> 'b option) -> 'b t

(** Same as [convert_partial], but the [decode] function is full. *)
val convert: 'a t -> encode: ('b -> 'a) -> decode: ('a -> 'b) -> 'b t

(** {2 Backward Compatibility} *)

(** Make a type description that changes from a given version.

    If the value being decoded is using a version which is strictly less than [from],
    it is decoded using [old]. Otherwise, it is decoded using [current].

    Example:
    [
      let v2 = string in
      let old = convert v2 ~encode: Int32.to_string ~decode: Int32.of_string_opt in
      versions ~old ~from: 3 int32
    ]
    In this example, one was using strings to encode integers, but one realized
    that it was a mistake and starting from version 3, integers are encoded as integers.

    It is typical to chain conversions to support older versions without having
    to encode a quadratic number of conversions.
    Continuing our example, if we realize that we actually need floats starting
    from version 4, here it what we can do:
    [
      let v3 =
        let v2 = string in
        let old = convert v2 ~encode: Int32.to_string ~decode: Int32.of_string_opt in
        versions ~old ~from: 3 int32
      in
      let old = convert v3 ~encode: int_of_float ~decode: (fun i -> Some (float_of_int i)) in
      versions ~old ~from: 4 float
    ]

    You can actually chain the other way around too, if you want to provide
    a direct conversion from older versions:
    [
      let v2 = string in
      let old = convert v2 ~encode: string_of_float ~decode: float_of_string_opt in
      versions ~old ~from: 3 @@
      let v3 = int32 in
      let old =
        convert v3 ~encode: Int32.of_float ~decode: (fun i -> Some (Int32.to_float i))
      in
      versions ~old ~from: 4 float
    ]
    Decoding version 2 is more efficient, but updating all old encodings like this
    at each new version can be time-consuming and error-prone. *)
val versions: old: 'a t -> from: version -> 'a t -> 'a t

(** Backward compatibility issue kinds. *)
type issue_kind =
  | Incompatible_types of string * string (** e.g. "int" vs "string", "list" vs "record"... *)
  | Ignored_field of Id.t (** an old record field does not appear in the new type *)
  | Ignored_enum of Id.t (** an old enum case does not appear in the new type *)
  | Ignored_variant of Id.t (** an old variant case does not appear in the new type *)
  | Missing_field of Id.t (** a new mandatory record field did not exist in the old type *)

(** Locations in values. *)
type location =
  | This
  | Option_value of location
  | Array_item of location
  | List_item of location
  | Record_field of Id.t * location
  | Variant_parameter of Id.t * location

(** Convert a location into text. *)
val show_location: location -> string

(** Backward compatibility issues. *)
type issue =
  {
    location: location;
    kind: issue_kind;
  }

(** Convert a backward compatibility issue into text. *)
val show_issue: issue -> string

(** Check backward compatibility between two types.

    Usage: [backward_compatible ~old ~old_version current]

    Check that all values encoded using [old] with version [old_version]
    can still be read using [current].

    This does not check that they would still make sense semantically.
    For instance, it does not check that default values are the same,
    or that the values that are associated to enum identifiers are the same
    or semantically equivalent.

    This check makes strong assumptions on the encoding, such as:
    - the encoding for a value does not depend on the surrounding value;
    - the encoding can rebuild identifiers without relying on the order in
      which they are declared (e.g. they store the name directly
      or they use a (tag <-> name) table);
    - field order for records is irrelevant;
    - record fields can only occur once;
    - renamings ([rename] information) are used to decode old names. *)
val backward_compatible: old: 'a t -> old_version: version -> 'b t -> issue list

(** {2 Some Uses of Types} *)

(** Output modes for [output_value]. *)
type output_mode =
  | Compact (** Output only spaces that are required to be valid OCaml. *)
  | One_line (** Output spaces but no line breaks and no indentation. *)
  | Multiline of int (** Output spaces and line breaks. Parameter is toplevel indentation. *)

(** Contexts for [output_value]. *)
type output_context =
  | Parentheses (** Value is a toplevel value or is inside parentheses or equivalent. *)
  | Variant (** Value is the parameter of a variant constructor. *)
  | Record (** Value is the value a record field. *)

(** Output a value using OCaml syntax.

    Usage: [output_value out typ value]

    Output [value] which is of type [typ] using output function [out].
    The output function will be called with strings to append to the output.
    A typical value for [out] is [print_string] or [Buffer.add_string] applied
    to a buffer (this is what [show_value] does).

    Default [mode] is [Multiline 0].
    Default [context] is [Parentheses].

    Note that [Convert]ed types will display values of the underlying type,
    so the result may not actually be usuable as an OCaml expression
    for type ['a]. *)
val output_value: ?mode: output_mode -> ?context: output_context ->
  (string -> unit) -> 'a t -> 'a -> unit

(** Convert a value to a string using OCaml syntax.

    This uses [output_value] so it has the same behavior. *)
val show_value: ?mode: output_mode -> ?context: output_context ->
  'a t -> 'a -> string

(** Generate an example value that has a given type.

    For enums and variants, the first case is chosen.

    If [empty] is [true], lists, arrays and options are generated empty.
    Otherwise, they are generated with one element. Default is [false].

    @raise [Invalid_argument] if given an empty [Enum] or [Variant] list
    of cases, or if given [Convert] such that [decode] cannot convert
    the example generated by the lower-level type. *)
val example_value: ?empty: bool -> 'a t -> 'a

(** Make new value that is a copy of an existing one.

    This can be useful to get a copy of a mutable value for instance.

    Note that if the [encode] and [decode] functions of [Convert] types
    do not preserve all information, only preserved information is copied,
    the rest gets the values that are given by [decode].
    Similarly, if those functions do not create new values but actually
    introduce sharing, the copy may share some values with other existing values.

    Also note that [Enum] values are not copied: the original value is returned.

    @raise [Invalid_arg] if the [decode] function of a [Convert] is not capable
    of decoding a value which was obtained with the corresponding [encode]. *)
val copy_value: 'a t -> 'a -> 'a
