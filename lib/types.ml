module Symbol = struct
  type octave = Higher_octave | Lower_octave [@@deriving show]

  type t = Swaram of char * octave option | Sustain | Rest | Lyrics of string
  [@@deriving show]
end

type matra_part = { symbols : Symbol.t list; instrument : string option }
[@@deriving show]
