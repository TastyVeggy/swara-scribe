module Error = struct
  type t = Parse_error of string

  let pp fmt (Parse_error msg) = Format.fprintf fmt "Parser error: %s" msg
end

let to_parser_token = function
  | Lexer.Token.LETTER c -> Parser.LETTER c
  | Lexer.Token.APOSTROPHE -> Parser.APOSTROPHE
  | Lexer.Token.COMMA -> Parser.COMMA
  | Lexer.Token.DASH -> Parser.DASH
  | Lexer.Token.FULL_STOP -> Parser.FULL_STOP
  | Lexer.Token.BARLINE -> Parser.BARLINE
  | Lexer.Token.WHITESPACE -> Parser.WHITESPACE
  | Lexer.Token.NEWLINE -> Parser.NEWLINE
  | Lexer.Token.INSTRUMENT_TEXT i -> Parser.INSTRUMENT_TEXT i
  | Lexer.Token.LYRICS s -> Parser.LYRICS s
  | Lexer.Token.EOF -> Parser.EOF

let parse tokens =
  let buf = ref tokens in
  let supplier () =
    match !buf with
    | [] -> (Parser.EOF, Lexing.dummy_pos, Lexing.dummy_pos)
    | t :: rest ->
        buf := rest;
        (to_parser_token t, Lexing.dummy_pos, Lexing.dummy_pos)
  in
  let parser_engine =
    MenhirLib.Convert.Simplified.traditional2revised Parser.score
  in
  try Ok (parser_engine supplier)
  with Parser.Error -> Error (Error.Parse_error "Unexpected token")
