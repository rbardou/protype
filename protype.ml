module Id:
sig
  type t
  val make: string -> t
  val name: t -> string
  val tag: t -> int
  val compare: t -> t -> int
end =
struct
  type t = { name: string; tag: int }

  let next_tag = ref 0

  module String_map = Map.Make (String)

  let known = ref String_map.empty

  let make name =
    match String_map.find_opt name !known with
      | Some id ->
          id
      | None ->
          let tag = !next_tag in
          incr next_tag;
          let id = { name; tag } in
          known := String_map.add name id !known;
          id

  let name id = id.name
  let tag id = id.tag
  let compare a b = a.tag - b.tag
end

let id = Id.make

module Id_set = Set.Make (Id)
module Id_map = Map.Make (Id)

let make_id_map list =
  List.fold_left (fun acc (k, v) -> Id_map.add k v acc) Id_map.empty list

module Version = Int
type version = Version.t
module Version_map = Map.Make (Int)

type _ annotation = ..

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
      from: Version.t;
      current: 'a t;
    } -> 'a t
  | Annotate: 'a annotation * 'a t -> 'a t
  | Recursive: ('a t -> 'a t) -> 'a t
  | Expanded_recursive: int * ('a t -> 'a t) -> 'a t

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

and ('v, 'a) case =
  {
    id: Id.t;
    typ: 'a t;
    make: 'a -> 'v;
  }

and 'v packed_case = Case: ('v, _) case -> 'v packed_case

and 'v packed_case_value = Value: ('v, 'a) case * 'a -> 'v packed_case_value

type e = E: _ t -> e

let unit = Unit
let bool = Bool
let char = Char
let int = Int
let int32 = Int32
let int64 = Int64
let float = Float
let string = String
let option t = Option t
let array t = Array t
let list t = List t

type ('r, 'f, 'g) partial_record = ('r, 'f) fields * 'g

let record ?(can_ignore = []) ?(rename = []) (fields, make) =
  let can_ignore = Id_set.of_list (List.map id can_ignore) in
  let rename = List.map (fun (a, b) -> id a, id b) rename in
  let rename = make_id_map rename in
  Record { fields; make; can_ignore; rename }

let (@) (name, typ, get) (next, make) =
  R_field { id = id name; typ; get; default = None; next }, make

let (@?) (name, typ, default, get) (next, make) =
  R_field { id = id name; typ; get; default = Some default; next }, make

let (@:) (name, typ, get) make =
  R_last { id = id name; typ; get; default = None }, make

let (@?:) (name, typ, default, get) make =
  R_last { id = id name; typ; get; default = Some default }, make

let enum ?(rename = []) cases id =
  let cases = make_id_map cases in
  let rename = make_id_map rename in
  Enum { cases; id; rename }

let case name typ make =
  { id = id name; typ; make }

let case_unit name value =
  case name unit (fun () -> value)

let value case parameter =
  Value (case, parameter)

let variant ?(rename = []) cases get =
  let cases =
    let add_case acc (Case { id; _ } as case) = Id_map.add id case acc in
    List.fold_left add_case Id_map.empty cases
  in
  let rename = List.map (fun (a, b) -> id a, id b) rename in
  let rename = make_id_map rename in
  Variant { cases; get; rename }

let convert_partial typ ~encode ~decode =
  Convert { typ; encode; decode }

let convert typ ~encode ~decode =
  convert_partial typ ~encode ~decode: (fun x -> Some (decode x))

let versions ~old ~from current =
  Versions { old; from; current }

type issue_kind =
  | Incompatible_types of string * string
  | Ignored_field of Id.t
  | Ignored_enum of Id.t
  | Ignored_variant of Id.t
  | Missing_field of Id.t

type location =
  | This
  | Option_value of location
  | Array_item of location
  | List_item of location
  | Record_field of Id.t * location
  | Variant_parameter of Id.t * location

let rec show_location location =
  match location with
    | This ->
        "root"
    | Option_value This ->
        "Some"
    | Option_value loc ->
        "Some." ^ show_location loc
    | Array_item This ->
        "[]"
    | Array_item loc ->
        "[]." ^ show_location loc
    | List_item This ->
        "[]"
    | List_item loc ->
        "[]." ^ show_location loc
    | Record_field (id, This)
    | Variant_parameter (id, This) ->
        Printf.sprintf "%S" (Id.name id)
    | Record_field (id, loc)
    | Variant_parameter (id, loc) ->
        Printf.sprintf "%S.%s" (Id.name id) (show_location loc)

type issue =
  {
    location: location;
    kind: issue_kind;
  }

let next_placeholder = ref 0

let fresh_placeholder () =
  let i = !next_placeholder in
  incr next_placeholder;
  i

let expand_recursive f =
  Expanded_recursive (fresh_placeholder (), f)

let rec show_base_type: 'a. _ -> 'a t -> _ = fun (type a) version (typ: a t) ->
  match typ with
    | Unit -> "unit"
    | Bool -> "bool"
    | Char -> "char"
    | Int -> "int"
    | Int32 -> "int32"
    | Int64 -> "int64"
    | Float -> "float"
    | String -> "string"
    | Option _ -> "option"
    | Array _ -> "array"
    | List _ -> "list"
    | Record _ -> "record"
    | Enum _ -> "enum"
    | Variant _ -> "variant"
    | Convert { typ; _ } -> show_base_type version typ
    | Versions { current; _ } -> show_base_type version current
    | Annotate (_, typ) -> show_base_type version typ
    | Recursive f -> show_base_type version (f (expand_recursive f))
    | Expanded_recursive (i, _) -> "rec#" ^ string_of_int i

let show_issue { location; kind } =
  let location = show_location location in
  match kind with
    | Incompatible_types (a, b) ->
        Printf.sprintf "%s used to have type %s, but now has type %s" location a b
    | Ignored_field id ->
        Printf.sprintf "%s used to have a field named %s" location (Id.name id)
    | Ignored_enum id ->
        Printf.sprintf "%s used to have an enum case named %s" location (Id.name id)
    | Ignored_variant id ->
        Printf.sprintf "%s used to have a variant constructor named %s" location (Id.name id)
    | Missing_field id ->
        Printf.sprintf "%s used to not have the now mandatory field named %s"
          location (Id.name id)

let with_location update issues =
  let update_issue issue =
    {
      location = update issue.location;
      kind = issue.kind;
    }
  in
  List.map update_issue issues

let issue kind = [ { location = This; kind } ]

let apply_rename rename id =
  match Id_map.find_opt id rename with
    | None -> id
    | Some id -> id

let apply_rename_to_map rename map =
  Id_map.bindings map
  |> List.map (fun (id, value) -> apply_rename rename id, value)
  |> make_id_map

let rec field_map: 'r 'f. _ -> _ -> ('r, 'f) fields -> _ =
  fun (type r) (type f) rename acc (fields: (r, f) fields) ->
  match fields with
    | R_field { id; typ; default; next; _ } ->
        let id = apply_rename rename id in
        field_map rename (Id_map.add id (E typ, default <> None) acc) next
    | R_last { id; typ; default; _ } ->
        let id = apply_rename rename id in
        Id_map.add id (E typ, default <> None) acc

let rec backward_compatible: 'a 'b. old: 'a t -> old_version: _ -> 'b t -> issue list =
  fun (type a) (type b) ~(old: a t) ~old_version (current: b t) ->
  let incompatible_types () =
    issue (
      Incompatible_types (show_base_type old_version old, show_base_type old_version current)
    )
  in
  match old, current with
    | Unit, Unit
    | Bool, Bool
    | Char, Char
    | Int, Int
    | Int32, Int32
    | Int64, Int64
    | Float, Float
    | String, String ->
        []
    | Unit, _
    | Bool, _
    | Char, _
    | Int, _
    | Int32, _
    | Int64, _
    | Float, _
    | String, _ ->
        incompatible_types ()
    | Option a, Option b ->
        with_location (fun l -> Option_value l) @@
        backward_compatible ~old: a ~old_version b
    | Option _, _ ->
        incompatible_types ()
    | Array a, Array b ->
        with_location (fun l -> Array_item l) @@
        backward_compatible ~old: a ~old_version b
    | Array _, _ ->
        incompatible_types ()
    | List a, List b ->
        with_location (fun l -> List_item l) @@
        backward_compatible ~old: a ~old_version b
    | List _, _ ->
        incompatible_types ()
    | Record {
        fields = old_fields;
        make = _;
        can_ignore = _;
        rename = _;
      },
      Record {
        fields = new_fields;
        make = _;
        can_ignore;
        rename;
      } ->
        backward_compatible_records old_version old_fields new_fields can_ignore rename
    | Record _, _ ->
        incompatible_types ()
    | Enum {
        cases = old_cases;
        id = _;
        rename = _;
      },
      Enum {
        cases = new_cases;
        id = _;
        rename;
      } ->
        backward_compatible_enums old_cases new_cases rename
    | Enum _, _ ->
        incompatible_types ()
    | Variant {
        cases = old_cases;
        get = _;
        rename = _;
      },
      Variant {
        cases = new_cases;
        get = _;
        rename;
      } ->
        backward_compatible_variants old_version old_cases new_cases rename
    | Variant _, _ ->
        incompatible_types ()
    | Convert { typ = old; _ }, _ ->
        backward_compatible ~old ~old_version current
    | _, Convert { typ = current; _ } ->
        backward_compatible ~old ~old_version current
    | Versions { current = old; _ }, _ ->
        backward_compatible ~old ~old_version current
    | _, Versions { current; _ } ->
        backward_compatible ~old ~old_version current
    | Annotate (_, old), _ ->
        backward_compatible ~old ~old_version current
    | _, Annotate (_, current) ->
        backward_compatible ~old ~old_version current
    | Recursive f, Recursive g ->
        (* Unify those two type variables by giving them the same placeholder. *)
        let placeholder = fresh_placeholder () in
        backward_compatible ~old: (Expanded_recursive (placeholder, f)) ~old_version
          (Expanded_recursive (placeholder, g))
    | Recursive f, _ ->
        backward_compatible ~old: (expand_recursive f) ~old_version current
    | _, Recursive f ->
        backward_compatible ~old ~old_version (expand_recursive f)
    | Expanded_recursive (a, _), Expanded_recursive (b, _) ->
        if a <> b then
          incompatible_types ()
        else
          []
    | Expanded_recursive _, _ ->
        incompatible_types ()

and backward_compatible_records:
  'ra 'fa 'rb 'fb. _ -> ('ra, 'fa) fields -> ('rb, 'fb) fields -> _ -> _ -> _ =
  fun old_version old_fields new_fields can_ignore rename ->
  let old_fields = field_map rename Id_map.empty old_fields in
  let new_fields = field_map rename Id_map.empty new_fields in
  let merge_fields id a b =
    match a, b with
      | None, None
      | None, Some (_, true) ->
          None
      | None, Some (_, false) ->
          Some (issue (Missing_field id))
      | Some _, None ->
          if Id_set.mem id can_ignore then
            None
          else
            Some (issue (Ignored_field id))
      | Some (E a, _), Some (E b, _) ->
          Some (
            with_location (fun l -> Record_field (id, l)) @@
            backward_compatible ~old: a ~old_version b
          )
  in
  Id_map.merge merge_fields old_fields new_fields
  |> Id_map.bindings |> List.map snd |> List.flatten

and backward_compatible_enums:
  'a 'b. 'a Id_map.t -> 'b Id_map.t -> _ -> _ =
  fun old_cases new_cases rename ->
  let old_cases = apply_rename_to_map rename old_cases in
  let merge_cases id a b =
    match a, b with
      | None, None
      | None, Some _
      | Some _, Some _ -> None
      | Some _, None -> Some (issue (Ignored_enum id))
  in
  Id_map.merge merge_cases old_cases new_cases
  |> Id_map.bindings |> List.map snd |> List.flatten

and backward_compatible_variants:
  'a 'b. _ -> 'a packed_case Id_map.t -> 'b packed_case Id_map.t -> _ -> _ =
  fun old_version old_cases new_cases rename ->
  let old_cases = apply_rename_to_map rename old_cases in
  let merge_cases id a b =
    match a, b with
      | None, None
      | None, Some _ ->
          None
      | Some _, None ->
          Some (issue (Ignored_variant id))
      | Some (Case { typ = a; _ }), Some (Case { typ = b; _ }) ->
          Some (
            with_location (fun l -> Variant_parameter (id, l)) @@
            backward_compatible ~old: a ~old_version b
          )
  in
  Id_map.merge merge_cases old_cases new_cases
  |> Id_map.bindings |> List.map snd |> List.flatten

let annotate annotation typ =
  Annotate (annotation, typ)

type 'a annotation_equality =
  {
    equal: 'b. 'b -> 'b annotation -> 'a option;
  }

let find_all (type a) (type b) (annotation_equality: a annotation_equality)
    (typ: b t) (value: b) =
  let rec find_all: 'c. a list -> 'c t -> 'c -> a list =
    fun (type c) (acc: a list) (typ: c t) (value: c) ->
      match typ with
        | Unit | Bool | Char | Int | Int32 | Int64 | Float | String | Enum _ ->
            acc
        | Option t ->
            (
              match value with
                | None ->
                    acc
                | Some x ->
                    find_all acc t x
            )
        | Array t ->
            Array.fold_left (fun acc x -> find_all acc t x) acc value
        | List t ->
            List.fold_left (fun acc x -> find_all acc t x) acc value
        | Record { fields; _ } ->
            let rec find_all_in_fields: 'f. a list -> (c, 'f) fields -> a list =
              fun (type f) (acc: a list) (fields: (c, f) fields) ->
                match fields with
                  | R_field { typ; get; next; _ } ->
                      let acc = find_all acc typ (get value) in
                      find_all_in_fields acc next
                  | R_last { typ; get; _ } ->
                      find_all acc typ (get value)
            in
            find_all_in_fields acc fields
        | Variant { get; _ } ->
            let Value ({ typ; _ }, x) = get value in
            find_all acc typ x
        | Convert { typ; encode; _ } ->
            find_all acc typ (encode value)
        | Versions { current; _ } ->
            find_all acc current value
        | Annotate (annotation, typ) ->
            (
              match annotation_equality.equal value annotation with
                | None ->
                    find_all acc typ value
                | Some found ->
                    found :: acc
            )
        | Recursive f | Expanded_recursive (_, f) ->
            find_all acc (f typ) value
  in
  find_all [] typ value

let recursive f = Recursive f

type output_mode =
  | Compact
  | One_line
  | Multiline of int

type output_context =
  | Parentheses
  | Variant
  | Record

let rec output_value: 'a. ?mode: _ -> ?context: _ -> (string -> unit) -> 'a t -> 'a -> _ =
  fun (type a) ?(mode = Multiline 0) ?(context = Parentheses)
    out (typ: a t) (value: a) ->
    let output_variant name typ value =
      let parentheses =
        match context with
          | Parentheses | Record -> false
          | Variant -> true
      in
      if parentheses then out "(";
      (
        match mode with
          | Compact ->
              out name;
              output_value ~mode: Compact ~context: Variant out typ value
          | _ ->
              out name;
              out " ";
              output_value ~mode ~context: Variant out typ value
      );
      if parentheses then out ")"
    in
    let output_array_or_list left right iteri length typ values =
      out left;
      if length > 0 then (
        match mode with
          | Compact ->
              iteri
                (fun i x -> out (if i > 0 then ";" else ""); output_value ~mode out typ x)
                values
          | One_line ->
              iteri
                (fun i x -> out (if i > 0 then "; " else " "); output_value ~mode out typ x)
                values;
              out " "
          | Multiline indent ->
              if length = 1 then
                (
                  out " ";
                  iteri (fun _ x -> output_value ~mode out typ x) values;
                  out " ";
                )
              else
                let inner_indent = indent + 2 in
                let inner_mode = Multiline inner_indent in
                let inner_indent_string = String.make inner_indent ' ' in
                let output_item _ x =
                  out "\n";
                  out inner_indent_string;
                  output_value ~mode: inner_mode out typ x;
                  out ";";
                in
                iteri output_item values;
                out "\n";
                out (String.make indent ' ')
      );
      out right
    in
    let output_number show suffix zero value =
      let output ~parentheses ~space =
        if space then out " ";
        if parentheses then out "(";
        out (show value);
        out suffix;
        if parentheses then out ")"
      in
      match mode, context with
        | _, Parentheses
        | (One_line | Multiline _), Record->
            output ~parentheses: false ~space: false
        | Compact, Variant ->
            if value < zero then
              output ~parentheses: true ~space: false
            else
              output ~parentheses: false ~space: true
        | Compact, Record ->
            if value < zero then
              output ~parentheses: false ~space: true
            else
              output ~parentheses: false ~space: false
        | (One_line | Multiline _), Variant ->
            output ~parentheses: (value < zero) ~space: true
    in
    let output_space_if_compact_variant () =
      match mode, context with
        | Compact, Variant -> out " "
        | (One_line | Multiline _), _
        | _, (Parentheses | Record) -> ()
    in
    match typ with
      | Unit ->
          out "()"
      | Bool ->
          output_space_if_compact_variant ();
          out (string_of_bool value)
      | Char ->
          out "'";
          out (String.escaped (String.make 1 value));
          out "'"
      | Int ->
          output_number string_of_int "" 0 value
      | Int32 ->
          output_number Int32.to_string "l" 0l value
      | Int64 ->
          output_number Int64.to_string "L" 0L value
      | Float ->
          output_number string_of_float "" 0. value
      | String ->
          out "\"";
          out (String.escaped value);
          out "\""
      | Option typ ->
          (
            match value with
              | None ->
                  output_space_if_compact_variant ();
                  out "None"
              | Some value ->
                  output_variant "Some" typ value
          )
      | Array typ ->
          output_array_or_list "[|" "|]" Array.iteri (Array.length value) typ value
      | List typ ->
          let length =
            match value with
              | [] -> 0
              | [ _ ] -> 1
              | _ :: _ :: _ -> 2
          in
          output_array_or_list "[" "]" List.iteri length typ value
      | Record { fields; _ } ->
          let output_space_or_indent more_indent =
            match mode with
              | Compact ->
                  ()
              | One_line ->
                  out " "
              | Multiline indent ->
                  out "\n";
                  out (String.make (indent + more_indent) ' ')
          in
          let equal =
            match mode with
              | Compact -> "="
              | One_line | Multiline _ -> " = "
          in
          let inner_mode =
            match mode with
              | Compact -> Compact
              | One_line -> One_line
              | Multiline indent -> Multiline (indent + 2)
          in
          let rec output_fields: 'f. (a, 'f) fields -> _ =
            fun (type f) (fields: (a, f) fields) ->
              match fields with
                | R_field { id; typ; get; next; _ } ->
                    out (Id.name id);
                    out equal;
                    output_value ~mode: inner_mode ~context: Record out typ (get value);
                    out ";";
                    output_space_or_indent 2;
                    output_fields next
                | R_last { id; typ; get; _ } ->
                    out (Id.name id);
                    out equal;
                    output_value ~mode: inner_mode ~context: Record out typ (get value);
                    match mode with
                      | Compact | One_line -> ()
                      | Multiline _ -> out ";"
          in
          out "{";
          output_space_or_indent 2;
          output_fields fields;
          output_space_or_indent 0;
          out "}"
      | Enum { id; _ } ->
          output_space_if_compact_variant ();
          out (Id.name (id value))
      | Variant { get; _ } ->
          let Value ({ id; typ; _ }, value) = get value in
          (
            match typ with
              | Unit ->
                  output_space_if_compact_variant ();
                  out (Id.name id)
              | _ ->
                  output_variant (Id.name id) typ value
          )
      | Convert { typ; encode; _ } ->
          output_value ~mode ~context out typ (encode value)
      | Versions { current; _ } ->
          output_value ~mode ~context out current value
      | Annotate (_, typ) ->
          output_value ~mode ~context out typ value
      | Recursive f | Expanded_recursive (_, f) ->
          output_value ~mode ~context out (f typ) value

let show_value ?mode ?context typ value =
  let buffer = Buffer.create 512 in
  output_value ?mode ?context (Buffer.add_string buffer) typ value;
  Buffer.contents buffer

let rec example_value: 'a. empty: bool -> 'a t -> 'a =
  fun (type a) ~empty (typ: a t) ->
  match typ with
    | Unit ->
        ((): a)
    | Bool ->
        false
    | Char ->
        'x'
    | Int ->
        0
    | Int32 ->
        0l
    | Int64 ->
        0L
    | Float ->
        0.
    | String ->
        ""
    | Option t ->
        if empty then
          None
        else
          Some (example_value ~empty t)
    | Array t ->
        if empty then
          ([| |]: a)
        else
          ([| example_value ~empty t |]: a)
    | List t ->
        if empty then
          []
        else
          [ example_value ~empty t ]
    | Record { fields; make; _ } ->
        let rec example_fields: 'f. (a, 'f) fields -> 'f -> a =
          fun (type f) (fields: (a, f) fields) (make: f) ->
            match fields with
              | R_field { typ; next; _ } ->
                  example_fields next (make (example_value ~empty typ))
              | R_last { typ; _ } ->
                  make (example_value ~empty typ)
        in
        example_fields fields make
    | Enum { cases; _ } ->
        (
          match Id_map.min_binding_opt cases with
            | None ->
                invalid_arg "Protype.example_value: enum: cases are empty"
            | Some (_, value) ->
                value
        )
    | Variant { cases; _ } ->
        (
          match Id_map.min_binding_opt cases with
            | None ->
                invalid_arg "Protype.example_value: variant: cases are empty"
            | Some (_, Case { typ; make; _ }) ->
                make (example_value ~empty typ)
        )
    | Convert { typ; decode; _ } ->
        (
          match decode (example_value ~empty typ) with
            | None ->
                invalid_arg "Protype.example_value: convert: cannot convert generated example"
            | Some x ->
                x
        )
    | Versions { current; _ } ->
        example_value ~empty current
    | Annotate (_, typ) ->
        example_value ~empty typ
    | Recursive f | Expanded_recursive (_, f) ->
        example_value ~empty: true (f typ)

let example_value ?(empty = false) typ =
  example_value ~empty typ

(* TODO: we could have [Pure] types to indicate that some values need not be copied. *)
let rec copy_value: 'a. 'a t -> 'a -> 'a = fun (type a) (typ: a t) (value: a) ->
  match typ with
    | Unit | Bool | Char | Int | Int32 | Int64 | Float | String | Enum _ ->
        (* Note that we assume strings are immutable. *)
        (* For [Enum], we could get the id of [value] and find the value again in [cases],
           but this would not return a copy, so it would be useless. *)
        value
    | Option typ ->
        (
          match value with
            | None -> None
            | Some value -> Some (copy_value typ value)
        )
    | Array typ ->
        Array.map (copy_value typ) value
    | List typ ->
        List.map (copy_value typ) value
    | Record { fields; make; _ } ->
        let rec copy_fields: 'f. (a, 'f) fields -> 'f -> a =
          fun (type f) (fields: (a, f) fields) (make: f) ->
            match fields with
              | R_field { typ; get; next; _ } ->
                  copy_fields next (make (copy_value typ (get value)))
              | R_last { typ; get; _ } ->
                  make (copy_value typ (get value))
        in
        copy_fields fields make
    | Variant { get; _ } ->
        let Value ({ typ; make; _ }, parameter) = get value in
        make (copy_value typ parameter)
    | Convert { typ; encode; decode } ->
        (
          match decode (copy_value typ (encode value)) with
            | None ->
                invalid_arg "Protype.copy_value: Convert: decode returned None"
            | Some value ->
                value
        )
    | Versions { current; _ } ->
        copy_value current value
    | Annotate (_, typ) ->
        copy_value typ value
    | Recursive f | Expanded_recursive (_, f) ->
        copy_value (f typ) value
