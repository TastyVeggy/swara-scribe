module Symbol = struct
  type octave = Higher_octave | Lower_octave [@@deriving show]

  type t = Swaram of char * octave option | Sustain | Rest | Lyrics of string
  [@@deriving show]
end

type matra_part = { symbols : Symbol.t list; instrument : string option }
[@@deriving show]

module Cst = struct
  type element = Matra_Part of matra_part | Barline | Newline | Next_Part
  [@@deriving show]

  type t = element list [@@deriving show]
end

module Elab = struct
  type part = Cst.element list [@@deriving show]
  type line = part list [@@deriving show]
end

module Ast = struct
  type matra = matra_part list [@@deriving show]
  type element = Matra of matra | Barline [@@deriving show]

  type line = {
    starting_bar_no : int;
    no_of_bars : int;
    no_of_parts : int;
    content : element list;
  }
  [@@deriving show]

  type t = { content : line list; instrumentation : string list }
  [@@deriving show]
end

(* this is per page *)
module Layout_Tree = struct
  type position = { x : float; y : float } [@@deriving show]

  (*y at baseline, x at left of symbol*)
  type symbol = {
    symbol : Symbol.t;
    baseline_left : position;
    is_emphasised : bool;
  }
  [@@deriving show]

  (*y at baseline, x at middle*)
  type barline = {
    height : float;
    baseline_mid : position;
    is_emphasised : bool;
  }
  [@@deriving show]

  (*y at baseline, x at right of text*)
  type instrument = {
    text : string;
    baseline_right : position;
    is_emphasised : bool;
  }
  [@@deriving show]

  (*y at baseline, x at left of barno*)
  type bar_no = { text : string; baseline_left : position } [@@deriving show]

  type element =
    | LSymbol of symbol
    | LBarline of barline
    | LInstrument of instrument
    | LBarno of bar_no
  [@@deriving show]

  type page = { content : element list; width : float; height : float }
  [@@deriving show]

  type t = page list [@@deriving show]
end
