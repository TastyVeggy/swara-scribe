open Types

type element =
  | Matra_Part of matra_part
  | Instrument of string
  | Barline
  | Newline
  | Next_Part
[@@deriving show]

type t = element list [@@deriving show]
