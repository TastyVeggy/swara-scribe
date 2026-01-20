open Cairo
open Ir

let note_size = 1.2 *. Layout.note_width

(* may be good to refactor this to layout *)
let dot_radius = 0.003 *. Layout.scale_y
let dot_offset_below = 0.019 *. Layout.scale_y
let dot_offset_above = 0.019 *. Layout.scale_y
let bar_no_text_size = Layout.text_size *. 0.7

type position = Layout_Tree.position

let draw_music_symbol_centered (cr : Cairo.context) (c : char) (pos : position)
    =
  set_font_size cr note_size;
  let text = String.make 1 c in
  let ext = text_extents cr text in
  let x_centered = pos.x -. (ext.width /. 2.) -. ext.x_bearing in
  let y_centered = pos.y -. (ext.height /. 2.) -. ext.y_bearing in
  move_to cr x_centered y_centered;
  show_text cr text

let draw_music_symbol (cr : Cairo.context) (s : Layout_Tree.symbol) : unit =
  match s.symbol with
  | Swaram (c, octave) -> (
      draw_music_symbol_centered cr c s.centre;
      set_font_size cr note_size;
      match octave with
      | Some Symbol.Higher_octave ->
          arc cr s.centre.x
            (s.centre.y -. dot_offset_above)
            ~r:dot_radius ~a1:0. ~a2:(2. *. Float.pi);
          fill cr
      | Some Symbol.Lower_octave ->
          arc cr s.centre.x
            (s.centre.y +. dot_offset_below)
            ~r:dot_radius ~a1:0. ~a2:(2. *. Float.pi);
          fill cr
      | None -> ())
  | Rest -> draw_music_symbol_centered cr ',' s.centre
  | Sustain -> draw_music_symbol_centered cr '-' s.centre

let draw_barline (cr : Cairo.context) (barline : Layout_Tree.barline) =
  let y1 = barline.centre.y -. (barline.height /. 2.) in
  let y2 = barline.centre.y +. (barline.height /. 2.) in
  move_to cr barline.centre.x y1;
  line_to cr barline.centre.x y2;
  stroke cr

let draw_instrumentation (cr : Cairo.context)
    (instrument : Layout_Tree.instrument) =
  set_font_size cr Layout.text_size;
  let ext = text_extents cr instrument.text in
  let y = instrument.right.y -. (ext.height /. 2.) -. ext.y_bearing in
  let x = instrument.right.x -. ext.x_advance in
  move_to cr x y;
  show_text cr instrument.text

let draw_bar_no (cr : Cairo.context) (bar_no_elem : Layout_Tree.bar_no) =
  set_font_size cr bar_no_text_size;
  let ext = text_extents cr bar_no_elem.text in
  let y = bar_no_elem.centre.y -. (ext.height /. 2.) -. ext.y_bearing in
  let x = bar_no_elem.centre.x -. (ext.width /. 2.) -. ext.x_bearing in
  move_to cr x y;
  show_text cr bar_no_elem.text

let rec draw_score (ctx : Cairo.context) : Layout_Tree.element list -> unit =
  function
  | [] -> ()
  | LSymbol s :: rest ->
      draw_music_symbol (ctx : context) s;
      draw_score ctx rest
  | LBarline b :: rest ->
      draw_barline (ctx : context) b;
      draw_score ctx rest
  | LInstrument inst :: rest ->
      draw_instrumentation (ctx : context) inst;
      draw_score ctx rest
  | LBarno bar_no_elem :: rest ->
      draw_bar_no (ctx : context) bar_no_elem;
      draw_score ctx rest

let generate_score_png (filename : string) (layout : Layout_Tree.t) =
  let width = ceil layout.width in
  let height = ceil layout.height in

  let surface =
    Cairo.Image.create Cairo.Image.ARGB32 ~w:(int_of_float width)
      ~h:(int_of_float height)
  in
  let cr = Cairo.create surface in

  set_source_rgb cr 1. 1. 1.;
  paint cr;

  select_font_face cr "Courier";
  set_font_size cr note_size;

  set_source_rgb cr 0. 0. 0.;
  draw_score cr layout.content;

  Cairo.PNG.write surface filename;
  Cairo.Surface.finish surface
