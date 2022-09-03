let (%) = Int.logor
let (<<) = Int.shift_left
let (>>) = Int.shift_right
let (&) = Int.logand

let utf_8_string_of_unicode i =
  if i <= 0x007F
  then
    let b = Bytes.create 1 in
    Bytes.set_int8 b 0 i;
    Bytes.to_string b
  else if i <= 0x07FF
  then begin
    let five_high_bits = (i >> 6) & 0b11111 in
    let six_low_bits = (i & 0b111111) in
    let high = (0b11000000 % five_high_bits) << 8 in
    let low = (0b10000000 % six_low_bits) in
    let n = high % low in 
    let b = Bytes.create 2 in
    Bytes.set_int16_be b 0 n;
    Bytes.to_string b
  end
  else if i <= 0xFFFF
  then begin
    let four_high_bits = (i >> 12) & 0b1111 in
    let six_mid_bits = (i >> 6) & 0b111111 in
    let six_low_bits = i & 0b111111 in
    let high = (0b11100000 % four_high_bits) << 16 in
    let mid = (0b10000000 % six_mid_bits) << 8 in
    let low = (0b10000000 % six_low_bits) in
    let n = high % mid % low in
    let b = Bytes.create 3 in
    Bytes.set_int32_be b 0 (Int32.of_int n);
    Bytes.to_string b
  end
  else if i <= 0x10FFFF
  then
    let three_hh_bits = (i >> 18) & 0b111 in
    let six_hl_bits = (i >> 12) & 0b111111 in
    let six_lh_bits = (i >> 6) & 0b111111 in
    let six_ll_bits = i & 0b111111 in
    let hh = (0b11110000 % three_hh_bits) << 24 in
    let hl = (0b10000000 % six_hl_bits) << 16 in
    let lh = (0b10000000 % six_lh_bits) << 8 in
    let ll = (0b10000000 % six_ll_bits) in
    let n = hh % hl % lh % ll in
    let b = Bytes.create 4 in
    Bytes.set_int32_be b 0 (Int32.of_int n);
    Bytes.to_string b
  else failwith (Printf.sprintf "invalid code point %X" i)

(* receices a string like "\\x41" or "\\u03bb" and return
   the an utf8 encoded string like "A" or "λ" *)
let unescape str =
  let escape_chars_len = (match str.[1] with
    | 'u' -> 4
    | 'x' -> 2
    |  _ -> failwith "invalid escape sequence") in
  let escape_chars = String.sub str 2 escape_chars_len in
  let as_int = Printf.sprintf "0x%s" escape_chars 
               |> int_of_string_opt 
               |> function Some x -> x | None -> 
                 failwith (Printf.sprintf "bad escape string %s" escape_chars) in
  utf_8_string_of_unicode as_int

let source_character = [%sedlex.regexp? any]

let single_escape_character = [%sedlex.regexp? Chars {|'"\\bfnrtv|}]

let decimal_digit = [%sedlex.regexp? '0' .. '9']

let escape_character =
  [%sedlex.regexp? single_escape_character | decimal_digit | 'x' | 'u']

let unicode_escape_sequence =
  [%sedlex.regexp? 'u', hex_digit, hex_digit, hex_digit, hex_digit]

let hex_escape_sequence = [%sedlex.regexp? 'x', hex_digit, hex_digit]

let non_escape_character =
  [%sedlex.regexp? Sub (source_character, escape_character)]

let character_escape_sequence =
  [%sedlex.regexp? single_escape_character | non_escape_character]

let escape_sequence =
  [%sedlex.regexp? '\\', (character_escape_sequence | '0' | hex_escape_sequence | unicode_escape_sequence)]

let double_string_character =
  [%sedlex.regexp?
    (Sub (source_character, ('"' | '\\'))
    | escape_sequence)]

let string_literal =
  [%sedlex.regexp? '"', Star double_string_character, '"']

type string_token =
  | Double_quote
  | Escape of string
  | Point of string

let rec string_lexer buf acc =
  let lexeme = Sedlexing.Utf8.lexeme in
  match%sedlex buf with
  | '"' -> 
    string_lexer buf (Double_quote :: acc)
  | escape_sequence ->
    let s =  lexeme buf in
    string_lexer buf (Escape s :: acc)
  | Sub (source_character, ('"' | '\\')) ->
    let s =  lexeme buf in
    string_lexer buf (Point s :: acc)
  | eof -> List.rev acc
  | _ -> lexeme buf |> Printf.sprintf "Unexpected token %s\n" |> failwith

let string_of_listr_string_token tokens =
  let b = Buffer.create 1024 in
  let rec loop b = function
    | [] -> Buffer.contents b
    | hd :: tl ->
      let s = (match hd with
       | Escape s -> unescape s
       | Point s -> s
       | Double_quote -> "") in
      Buffer.add_string b s;
      loop b tl in
  loop b tokens

let main () =
  let lexbuf = Sedlexing.Utf8.from_string {|"Hello \u03bb \x77\x6F\x72\x6C\x64"|} in
  assert (string_lexer lexbuf [] |> string_of_listr_string_token = "Hello λ world")
