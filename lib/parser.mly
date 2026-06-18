%{
open Ast
open Types
%}

%token <char>   LETTER
%token <string> INSTRUMENT_TEXT
%token <string> LYRICS
%token APOSTROPHE COMMA DASH FULL_STOP
%token BARLINE WHITESPACE NEWLINE EOF

%start <Ast.t> score

%%

score:
  | elems = elements EOF { elems }

elements:
  | { [] }
  | WHITESPACE rest = elements
      { rest }
  | e = newline_run rest = elements_after_newlines
      { e :: rest }
  | mp = matra_part rest = elements_after_matra
      { Matra_Part { symbols = List.rev mp; instrument = None } :: rest }
  | e = element rest = elements
      { e :: rest }

elements_after_newlines:
  | { [] }
  | WHITESPACE rest = elements
      { rest }
  | mp = matra_part rest = elements_after_matra
      { Matra_Part { symbols = List.rev mp; instrument = None } :: rest }
  | e = element rest = elements
      { e :: rest }

elements_after_matra:
  | { [] }
  | WHITESPACE rest = elements
      { rest }
  | e = newline_run rest = elements_after_newlines
      { e :: rest }
  | e = element rest = elements
      { e :: rest }

newline_run:
  | NEWLINE { Next_Part }
  | NEWLINE NEWLINE trailing_newlines { Newline }

trailing_newlines:
  | { () }
  | NEWLINE trailing_newlines { () }

element:
  | i = INSTRUMENT_TEXT
      { Instrument i }
  | BARLINE
      { Barline }
  | s = LYRICS
      { Matra_Part { symbols = [ Symbol.Lyrics s ]; instrument = None } }

matra_part:
  | s = symbol { [ s ] }
  | mp = matra_part s = symbol { s :: mp }

symbol:
  | c = LETTER APOSTROPHE { Symbol.Swaram (c, Some Symbol.Higher_octave) }
  | c = LETTER COMMA      { Symbol.Swaram (c, Some Symbol.Lower_octave) }
  | c = LETTER            { Symbol.Swaram (c, None) }
  | DASH                  { Symbol.Sustain }
  | FULL_STOP             { Symbol.Rest }
