module type CODEC =
sig
  val encode: 'a Protype.t -> 'a -> string

  type decode_error
  val show_decode_error: decode_error -> string
  val decode: 'a Protype.t -> string -> ('a, decode_error) result
end

module Make (Codec: CODEC):
sig
  val run: unit -> unit
end
