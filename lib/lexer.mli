type position = { line : int; column : int }

module Token : sig
  type t =
    | LETTER of char
    | DASH (* sustain *)
    | APOSTROPHE (*high octave*)
    | COMMA (* low octave*)
    | WHITESPACE (* tempo marking *)
    | FULL_STOP (* rest *)
    | BARLINE (* barline *)
    | NEWLINE (* new line *)
    | LSQUAREBRACKET
    | RSQUAREBRACKET
    | INSTRUMENT_TEXT of string
    | EOF

  val pp : Format.formatter -> t -> unit
end

module Error : sig
  type t

  val pp : Format.formatter -> t -> unit
end

val lex : string -> (Token.t list, Error.t) result
