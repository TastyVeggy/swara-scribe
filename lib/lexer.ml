type position = { line : int; column : int }

module Token = struct
  type t =
    | LETTER of char
    | DASH (* sustain *)
    | APOSTROPHE (*high octave*)
    | COMMA (* low octave*)
    | WHITESPACE (* tempo marking *)
    | FULL_STOP (* rest *)
    | BARLINE (* barline *)
    | NEWLINE (* new line *)
    | LSQUAREBRACKET (* notating insrument *)
    | RSQUAREBRACKET (* notating instrument *)
    | INSTRUMENT_TEXT of string
    | LCURLYBRACKET (* matra marking for lyrics*)
    | RCURLYBRACKET (* matra marking for lyrics*)
    | LYRICS of string
    | EOF

  let pp fmt t =
    match t with
    | LETTER c -> Format.fprintf fmt "LETTER (%c)" c
    | DASH -> Format.fprintf fmt "DASH"
    | APOSTROPHE -> Format.fprintf fmt "APROSTROPHE"
    | COMMA -> Format.fprintf fmt "COMMA"
    | WHITESPACE -> Format.fprintf fmt "WHITESPACE"
    | FULL_STOP -> Format.fprintf fmt "FULL_STOP"
    | BARLINE -> Format.fprintf fmt "BAR"
    | NEWLINE -> Format.fprintf fmt "NEWLINE"
    | LSQUAREBRACKET -> Format.fprintf fmt "LSQUAREBRACKET"
    | RSQUAREBRACKET -> Format.fprintf fmt "RSQUAREBRACKET"
    | INSTRUMENT_TEXT s -> Format.fprintf fmt "INSTRUMENT_TEXT (%s)" s
    | LCURLYBRACKET -> Format.fprintf fmt "LCURLYBRACKET"
    | RCURLYBRACKET -> Format.fprintf fmt "RCURLYBRACKET"
    | LYRICS s -> Format.fprintf fmt "LYRICS (%s)" s
    | EOF -> Format.fprintf fmt "EOF"
end

module Error = struct
  type t =
    | Unexpected_char of char * position
    | Unclosed_bracket of char * position

  let pp fmt e =
    match e with
    | Unexpected_char (c, p) ->
        Format.fprintf fmt
          "Lexing error: Line %d, Character %d: '%c' is an invalid symbol."
          p.line p.column c
    | Unclosed_bracket (c, p) ->
        Format.fprintf fmt
          "Lexing error: Line %d: Unclosed bracket. Looking for %c" p.line c
end

let notes = "SRGMPDNrgmdn"

let lex (source : string) : (Token.t list, Error.t) result =
  let len = String.length source in

  let advance pos = { pos with column = pos.column + 1 } in
  let newline pos = { line = pos.line + 1; column = 1 } in
  let rec read_text (i : int) (pos : position) (close_symbol : char)
      (acc_buf : Buffer.t) : (string * int * position, Error.t) result =
    if i >= len then Error (Error.Unclosed_bracket (close_symbol, pos))
    else
      match source.[i] with
      | c when c = close_symbol ->
          Ok (Buffer.contents acc_buf, i + 1, advance pos)
      | '\n' as c ->
          Buffer.add_char acc_buf c;
          read_text (i + 1) (newline pos) close_symbol acc_buf
      | c ->
          Buffer.add_char acc_buf c;
          read_text (i + 1) (advance pos) close_symbol acc_buf
  in

  let rec lex_ i pos acc =
    if i >= len then Ok (List.rev (Token.EOF :: acc))
    else
      match source.[i] with
      | '\t' -> lex_ (i + 1) (advance pos) acc
      | '[' -> (
          match read_text (i + 1) (advance pos) ']' (Buffer.create 16) with
          | Ok (text, next_i, next_pos) ->
              lex_ next_i next_pos (Token.INSTRUMENT_TEXT text :: acc)
          | Error e -> Error e)
      | '{' -> (
          match read_text (i + 1) (advance pos) '}' (Buffer.create 16) with
          | Ok (text, next_i, next_pos) ->
              lex_ next_i next_pos (Token.LYRICS text :: acc)
          | Error e -> Error e)
      | c when String.contains notes c ->
          lex_ (i + 1) (advance pos) (Token.LETTER c :: acc)
      | '-' -> lex_ (i + 1) (advance pos) (Token.DASH :: acc)
      | '\'' -> lex_ (i + 1) (advance pos) (Token.APOSTROPHE :: acc)
      | ',' -> lex_ (i + 1) (advance pos) (Token.COMMA :: acc)
      | ' ' -> lex_ (i + 1) (advance pos) (Token.WHITESPACE :: acc)
      | '.' -> lex_ (i + 1) (advance pos) (Token.FULL_STOP :: acc)
      | '|' -> lex_ (i + 1) (advance pos) (Token.BARLINE :: acc)
      | '\n' -> lex_ (i + 1) (newline pos) (Token.NEWLINE :: acc)
      | invalid -> Error (Error.Unexpected_char (invalid, pos))
  in
  lex_ 0 { line = 1; column = 1 } []
