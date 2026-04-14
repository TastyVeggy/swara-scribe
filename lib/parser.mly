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
  | e = element rest = elements { e @ rest }

element:
  | i = INSTRUMENT_TEXT
      { [ Instrument i ] }
  | NEWLINE NEWLINE
      { [ Newline ] }
  | NEWLINE
      { [ Next_Part ] }
  | BARLINE
      { [ Barline ] }
  | WHITESPACE
      { [] }
  | s = LYRICS
      { [ Matra_Part { symbols = [ Symbol.Lyrics s ]; instrument = None } ] }
  | mp = matra_part
      { [ Matra_Part { symbols = mp; instrument = None } ] }

matra_part:
  | s = symbol { [ s ] }
  | s = symbol rest = matra_part { s :: rest }

symbol:
  | c = LETTER APOSTROPHE { Symbol.Swaram (c, Some Symbol.Higher_octave) }
  | c = LETTER COMMA      { Symbol.Swaram (c, Some Symbol.Lower_octave) }
  | c = LETTER            { Symbol.Swaram (c, None) }
  | DASH                  { Symbol.Sustain }
  | FULL_STOP             { Symbol.Rest }
