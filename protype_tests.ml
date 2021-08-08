module type CODEC =
sig
  val encode: 'a Protype.t -> 'a -> string

  type decode_error
  val show_decode_error: decode_error -> string
  val decode: 'a Protype.t -> string -> ('a, decode_error) result
end

module Make (Codec: CODEC) =
struct

  let echo x = Printf.ksprintf print_endline x

  let verbose = false

  let total_bytes = ref 0

  let hex_char_of_int = function
    | 0 -> '0'
    | 1 -> '1'
    | 2 -> '2'
    | 3 -> '3'
    | 4 -> '4'
    | 5 -> '5'
    | 6 -> '6'
    | 7 -> '7'
    | 8 -> '8'
    | 9 -> '9'
    | 10 -> 'a'
    | 11 -> 'b'
    | 12 -> 'c'
    | 13 -> 'd'
    | 14 -> 'e'
    | 15 -> 'f'
    | i -> invalid_arg ("hex_char_of_int " ^ string_of_int i)

  let to_hex s =
    String.init (String.length s * 2) @@ fun i ->
    if i mod 2 = 0 then
      hex_char_of_int (Char.code s.[i / 2] lsr 4)
    else
      hex_char_of_int (Char.code s.[i / 2] land 0xF)

  let check typ value =
    let encoded = Codec.encode typ value in
    total_bytes := !total_bytes + String.length encoded;
    match Codec.decode typ encoded with
      | Ok decoded ->
          let equal = decoded = value in
          if not equal then echo "error: not equal:";
          if not equal || verbose then
            echo "%s\n=> %S (%s)\n=> %s" (Protype.show_value typ value)
              encoded (to_hex encoded)
              (Protype.show_value typ decoded)
      | Error error ->
          echo "error: %s" (Codec.show_decode_error error)

  let tuple1 a =
    Protype.(
      record @@
      ("a", a, (fun x -> x)) @:
      (fun x -> x)
    )

  let tuple2 a b =
    Protype.(
      record @@
      ("a", a, (fun (x, _) -> x)) @
      ("b", b, (fun (_, x) -> x)) @:
      (fun a b -> a, b)
    )

  let tuple3 a b c =
    Protype.(
      record @@
      ("a", a, (fun (x, _, _) -> x)) @
      ("b", b, (fun (_, x, _) -> x)) @
      ("c", c, (fun (_, _, x) -> x)) @:
      (fun a b c -> a, b, c)
    )

  let tuple4 a b c d =
    Protype.(
      record @@
      ("a", a, (fun (x, _, _, _) -> x)) @
      ("b", b, (fun (_, x, _, _) -> x)) @
      ("c", c, (fun (_, _, x, _) -> x)) @
      ("d", d, (fun (_, _, _, x) -> x)) @:
      (fun a b c d -> a, b, c, d)
    )

  let tuple5 a b c d e default =
    Protype.(
      record @@
      ("a", a, (fun (x, _, _, _, _) -> x)) @
      ("b", b, (fun (_, x, _, _, _) -> x)) @
      ("c", c, (fun (_, _, x, _, _) -> x)) @
      ("d", d, (fun (_, _, _, x, _) -> x)) @
      ("e", e, default, (fun (_, _, _, _, x) -> x)) @?:
      (fun a b c d e -> a, b, c, d, e)
    )

  module Run (Check: sig val check: 'a Protype.t -> 'a -> unit val fast: bool end) =
  struct
    open Check

    let run () =
      check Protype.unit ();
      check Protype.int32 0l;
      check Protype.int32 1l;
      check Protype.int32 2l;
      check Protype.int32 3l;
      check Protype.int32 4l;
      check Protype.int32 5l;
      check Protype.int32 6l;
      check Protype.int32 7l;
      check Protype.int32 8l;
      check Protype.int32 15l;
      check Protype.int32 255l;
      check Protype.int32 256l;
      check Protype.int32 0x1234l;
      check Protype.int32 0xFFFFl;
      check Protype.int32 0x10000l;
      check Protype.int32 0x123456l;
      check Protype.int32 0xFFFFFFl;
      check Protype.int32 0x1000000l;
      check Protype.int32 0x12345678l;
      check Protype.int32 0xFFFFFFFFl;
      check Protype.int64 0x100000000L;
      check Protype.int64 0x1234567890L;
      check Protype.int64 0xFFFFFFFFFFL;
      check Protype.int64 0x10000000000L;
      check Protype.int64 0x123456789012L;
      check Protype.int64 0xFFFFFFFFFFFFL;
      check Protype.int64 0x1000000000000L;
      check Protype.int64 0x12345678901234L;
      check Protype.int64 0xFFFFFFFFFFFFFFL;
      check Protype.int64 0x100000000000000L;
      check Protype.int64 0x1234567890123456L;
      check Protype.int64 0xFFFFFFFFFFFFFFFFL;
      check Protype.int32 (-1l);
      check Protype.int32 (-2l);
      check Protype.int32 (-3l);
      check Protype.int32 (-4l);
      check Protype.int32 (-5l);
      check Protype.int32 (-6l);
      check Protype.int32 (-7l);
      check Protype.int32 (-8l);
      check Protype.int32 (-15l);
      check Protype.int32 (-255l);
      check Protype.int32 (-256l);
      check Protype.int32 (-0x1234l);
      check Protype.int32 (-0xFFFFl);
      check Protype.int32 (-0x10000l);
      check Protype.int32 (-0x123456l);
      check Protype.int32 (-0xFFFFFFl);
      check Protype.int32 (-0x1000000l);
      check Protype.int32 (-0x12345678l);
      check Protype.int32 (-0xFFFFFFFFl);
      check Protype.int64 (-0x100000000L);
      check Protype.int64 (-0x1234567890L);
      check Protype.int64 (-0xFFFFFFFFFFL);
      check Protype.int64 (-0x10000000000L);
      check Protype.int64 (-0x123456789012L);
      check Protype.int64 (-0xFFFFFFFFFFFFL);
      check Protype.int64 (-0x1000000000000L);
      check Protype.int64 (-0x12345678901234L);
      check Protype.int64 (-0xFFFFFFFFFFFFFFL);
      check Protype.int64 (-0x100000000000000L);
      check Protype.int64 (-0x1234567890123456L);
      check Protype.int64 (-0xFFFFFFFFFFFFFFFFL);
      check Protype.int 0;
      check Protype.int 1;
      check Protype.int 2;
      check Protype.int 3;
      check Protype.int 4;
      check Protype.int 5;
      check Protype.int 6;
      check Protype.int 7;
      check Protype.int 8;
      check Protype.int 15;
      check Protype.int 255;
      check Protype.int 256;
      check Protype.int 0x1234;
      check Protype.int 0xFFFF;
      check Protype.int 0x10000;
      check Protype.int 0x123456;
      check Protype.int 0xFFFFFF;
      check Protype.int 0x1000000;
      check Protype.int 0x12345678;
      check Protype.int 0xFFFFFFFF;
      check Protype.int (-1);
      check Protype.int (-2);
      check Protype.int (-3);
      check Protype.int (-4);
      check Protype.int (-5);
      check Protype.int (-6);
      check Protype.int (-7);
      check Protype.int (-8);
      check Protype.int (-15);
      check Protype.int (-255);
      check Protype.int (-256);
      check Protype.int (-0x1234);
      check Protype.int (-0xFFFF);
      check Protype.int (-0x10000);
      check Protype.int (-0x123456);
      check Protype.int (-0xFFFFFF);
      check Protype.int (-0x1000000);
      check Protype.int (-0x12345678);
      check Protype.int (-0xFFFFFFFF);
      check Protype.int64 0L;
      check Protype.int64 1L;
      check Protype.int64 2L;
      check Protype.int64 3L;
      check Protype.int64 4L;
      check Protype.int64 5L;
      check Protype.int64 6L;
      check Protype.int64 7L;
      check Protype.int64 8L;
      check Protype.int64 15L;
      check Protype.int64 255L;
      check Protype.int64 256L;
      check Protype.int64 0x1234L;
      check Protype.int64 0xFFFFL;
      check Protype.int64 0x10000L;
      check Protype.int64 0x123456L;
      check Protype.int64 0xFFFFFFL;
      check Protype.int64 0x1000000L;
      check Protype.int64 0x12345678L;
      check Protype.int64 0xFFFFFFFFL;
      check Protype.int64 (-1L);
      check Protype.int64 (-2L);
      check Protype.int64 (-3L);
      check Protype.int64 (-4L);
      check Protype.int64 (-5L);
      check Protype.int64 (-6L);
      check Protype.int64 (-7L);
      check Protype.int64 (-8L);
      check Protype.int64 (-15L);
      check Protype.int64 (-255L);
      check Protype.int64 (-256L);
      check Protype.int64 (-0x1234L);
      check Protype.int64 (-0xFFFFL);
      check Protype.int64 (-0x10000L);
      check Protype.int64 (-0x123456L);
      check Protype.int64 (-0xFFFFFFL);
      check Protype.int64 (-0x1000000L);
      check Protype.int64 (-0x12345678L);
      check Protype.int64 (-0xFFFFFFFFL);
      if not fast then (
        for _ = 0 to 100000 do
          check Protype.int64 (Int64.of_int (Random.int 1000000000));
        done;
      );
      check Protype.bool false;
      check Protype.bool true;
      check Protype.char '\000';
      check Protype.char '\001';
      check Protype.char 'x';
      check Protype.char ' ';
      check Protype.char '\254';
      check Protype.char '\255';
      if not fast then (
        for _ = 0 to 100000 do
          check Protype.float (Random.float 100000000.)
        done;
      );
      check Protype.string "";
      check Protype.string "a";
      check Protype.string "ab";
      check Protype.string "abc";
      check Protype.string "abcd";
      check Protype.string "abcde";
      check Protype.string "abcdef";
      check Protype.string "abcdefg";
      check Protype.string "abcdefgh";
      check Protype.string "abcdefghi";
      check Protype.string "abcdefghij";
      if not fast then (
        for _ = 0 to 100 do
          check Protype.string
            (String.init (Random.int 100) (fun _ -> Char.chr (Random.int 256)))
        done;
        for _ = 0 to 100 do
          check Protype.string
            (String.init (Random.int 1000) (fun _ -> Char.chr (Random.int 256)))
        done;
        for _ = 0 to 100 do
          check Protype.string
            (String.init (Random.int 100000) (fun _ -> Char.chr (Random.int 256)))
        done;
      );
      check Protype.(option int64) None;
      check Protype.(option int64) (Some 42L);
      check Protype.(list bool) [];
      check Protype.(list bool) [ true ];
      check Protype.(list bool) [ true; false ];
      check Protype.(array float) [| |];
      check Protype.(array float) [| 42. |];
      check Protype.(array float) [| 0.; -42. |];
      check (tuple1 Protype.int32) 5l;
      check (tuple2 Protype.float Protype.bool) (7.81223, true);
      check (tuple3 Protype.float Protype.bool Protype.string) (7.81223, true, "lalala");
      check (tuple4 Protype.unit Protype.float Protype.int32 Protype.int64)
        ((), 1.23456e8, 42l, 938791827L);
      check
        (tuple5 Protype.unit Protype.float Protype.int32 Protype.int64 Protype.string "lol")
        ((), 1.23456e8, 42l, 938791827L, "plop");
      let enum =
        Protype.enum
          (List.init 20 (fun i -> (Protype.Id.make (string_of_int i)), i))
          (fun i ->
             assert (i >= 0 && i < 20);
             Protype.Id.make (string_of_int i))
      in
      for i = 0 to 19 do check enum i done;
      let variant =
        let open Protype in
        let a = case "A" unit (fun x -> `A x) in
        let b = case "B" int32 (fun x -> `B x) in
        let c = case "C" float (fun x -> `C x) in
        let d = case "D" string (fun x -> `D x) in
        variant [ Case a; Case b; Case c; Case d ] @@ function
        | `A x -> value a x
        | `B x -> value b x
        | `C x -> value c x
        | `D x -> value d x
      in
      check variant (`A ());
      check variant (`B 15616l);
      check variant (`C 0.00000001);
      check variant (`D "");
      ()
  end

  module Run_base = Run (struct let check = check let fast = false end)

  let check_option typ value =
    check (Protype.option typ) None;
    check (Protype.option typ) (Some value)

  module Run_option = Run (struct let check = check_option let fast = false end)

  let check_option_option typ value =
    check (Protype.option (Protype.option typ)) None;
    check (Protype.option (Protype.option typ)) (Some None);
    check (Protype.option (Protype.option typ)) (Some (Some value))

  module Run_option_option = Run (struct let check = check_option_option let fast = false end)

  let check_array typ value =
    for i = 0 to 100 do
      check (Protype.array typ) (Array.init i (fun _ -> value))
    done

  module Run_array = Run (struct let check = check_array let fast = true end)

  let check_list typ value =
    for i = 0 to 100 do
      check (Protype.list typ) (List.init i (fun _ -> value))
    done

  module Run_list = Run (struct let check = check_list let fast = true end)

  let check_record typ value =
    check (tuple1 typ) value;
    check (tuple2 typ typ) (value, value);
    check (tuple3 typ typ typ) (value, value, value);
    check (tuple4 typ typ typ typ) (value, value, value, value)

  module Run_record = Run (struct let check = check_record let fast = true end)

  let check_variant typ v =
    let variant =
      let open Protype in
      let a = case "A" typ (fun x -> `A x) in
      let b = case "B" typ (fun x -> `B x) in
      let c = case "C" typ (fun x -> `C x) in
      variant [ Case a; Case b; Case c ] @@ function
      | `A x -> value a x
      | `B x -> value b x
      | `C x -> value c x
    in
    check variant (`A v);
    check variant (`B v);
    check variant (`C v)

  let check_big_list () =
    let typ = Protype.(list int) in
    let list = List.init 1_000_000 (fun x -> x) in
    check typ list

  module Run_variant = Run (struct let check = check_variant let fast = true end)

  let run () =
    Printexc.record_backtrace true;
    try
      let start = Unix.gettimeofday () in
      Run_base.run ();
      Run_option.run ();
      Run_option_option.run ();
      Run_array.run ();
      Run_list.run ();
      Run_record.run ();
      Run_variant.run ();
      check_big_list ();
      echo "Total number of bytes: %d in %gs" !total_bytes (Unix.gettimeofday () -. start);
    with exn ->
      Printexc.print_backtrace stdout;
      echo "Uncaught exception: %s" (Printexc.to_string exn);
      exit 1

end
