open Cairo
open Ir

let note_size = Layout.note_size
let dot_radius = 0.003 *. Layout.scale_y
let bottom_dot_offset = 0.013 *. Layout.scale_y
let top_dot_offset = 0.01 *. Layout.scale_y
let bar_no_text_size = Layout.text_size *. 0.7
let grey = (0.75, 0.75, 0.75)
let black = (0., 0., 0.)

type position = Layout_Tree.position

let set_colour (cr : Cairo.context) (is_emphasised : bool) =
  let r, g, b = if is_emphasised then black else grey in
  set_source_rgb cr r g b

(** pos.x is the left edge of the note slot, pos.y is the baseline*)
let note_centre_x (pos : position) = pos.x +. (Layout.note_width /. 2.)

let draw_music_symbol_at_baseline (cr : Cairo.context) (c : char)
    (pos : position) =
  set_font_size cr note_size;
  move_to cr pos.x pos.y;
  show_text cr (String.make 1 c)

let draw_text_at_baseline (cr : Cairo.context) (s : string) (pos : position) =
  set_font_size cr Layout.text_size;
  let ext = text_extents cr s in
  let x = note_centre_x pos -. (ext.width /. 2.) -. ext.x_bearing in
  move_to cr x pos.y;
  show_text cr s

let draw_music_symbol (cr : Cairo.context) (s : Layout_Tree.symbol) : unit =
  set_colour cr s.is_emphasised;
  match s.symbol with
  | Swaram (c, octave) -> (
      draw_music_symbol_at_baseline cr c s.baseline_left;
      set_font_size cr note_size;
      let cx = note_centre_x s.baseline_left in
      match octave with
      | Some Symbol.Higher_octave ->
          let dot_y =
            s.baseline_left.y -. Layout.note_ascent -. top_dot_offset
          in
          arc cr cx dot_y ~r:dot_radius ~a1:0. ~a2:(2. *. Float.pi);
          fill cr
      | Some Symbol.Lower_octave ->
          let dot_y = s.baseline_left.y +. bottom_dot_offset in
          arc cr cx dot_y ~r:dot_radius ~a1:0. ~a2:(2. *. Float.pi);
          fill cr
      | None -> ())
  | Rest -> draw_music_symbol_at_baseline cr ',' s.baseline_left
  | Sustain -> draw_music_symbol_at_baseline cr '-' s.baseline_left
  | Lyrics t -> draw_text_at_baseline cr t s.baseline_left

let draw_barline (cr : Cairo.context) (barline : Layout_Tree.barline) =
  set_colour cr barline.is_emphasised;
  let y1 = barline.baseline_mid.y -. (barline.height /. 2.) in
  let y2 = barline.baseline_mid.y +. (barline.height /. 2.) in
  move_to cr barline.baseline_mid.x y1;
  line_to cr barline.baseline_mid.x y2;
  stroke cr

let draw_instrumentation (cr : Cairo.context)
    (instrument : Layout_Tree.instrument) =
  set_colour cr instrument.is_emphasised;
  set_font_size cr Layout.text_size;
  let ext = text_extents cr instrument.text in
  let x = instrument.baseline_right.x -. ext.x_advance in
  move_to cr x instrument.baseline_right.y;
  show_text cr instrument.text

let draw_bar_no (cr : Cairo.context) (bar_no_elem : Layout_Tree.bar_no) =
  set_source_rgb cr 0. 0. 0.;
  set_font_size cr bar_no_text_size;
  let ext = text_extents cr bar_no_elem.text in
  let x = bar_no_elem.baseline_left.x -. (ext.width /. 2.) -. ext.x_bearing in
  move_to cr x bar_no_elem.baseline_left.y;
  show_text cr bar_no_elem.text

let draw_title (cr : Cairo.context) (t : Layout_Tree.title) =
  set_source_rgb cr 0. 0. 0.;
  set_font_size cr Layout.title_size;
  let ext = text_extents cr t.text in
  let x = t.baseline_mid.x -. (ext.width /. 2.) -. ext.x_bearing in
  move_to cr x t.baseline_mid.y;
  show_text cr t.text

let draw_page_no (cr : Cairo.context) (p : Layout_Tree.page_no) =
  set_source_rgb cr 0. 0. 0.;
  set_font_size cr Layout.page_no_size;
  let ext = text_extents cr p.text in
  let x = p.baseline_mid.x -. (ext.width /. 2.) -. ext.x_bearing in
  move_to cr x p.baseline_mid.y;
  show_text cr p.text

let rec draw_score (cr : Cairo.context) : Layout_Tree.element list -> unit =
  function
  | [] -> ()
  | LSymbol s :: rest ->
      draw_music_symbol cr s;
      draw_score cr rest
  | LBarline b :: rest ->
      draw_barline cr b;
      draw_score cr rest
  | LInstrument inst :: rest ->
      draw_instrumentation cr inst;
      draw_score cr rest
  | LBarno bar_no_elem :: rest ->
      draw_bar_no cr bar_no_elem;
      draw_score cr rest
  | LTitle t :: rest ->
      draw_title cr t;
      draw_score cr rest
  | LPageNo p :: rest ->
      draw_page_no cr p;
      draw_score cr rest

let generate_score_pdf (filename : string) (pages : Layout_Tree.t)
    (config : Config.config) =
  match pages with
  | [] -> ()
  | first :: _ ->
      let surface = Cairo.PDF.create filename ~w:first.width ~h:first.height in
      let cr = Cairo.create surface in
      select_font_face cr config.font;
      List.iter
        (fun (page : Layout_Tree.page) ->
          Cairo.PDF.set_size surface ~w:page.width ~h:page.height;
          set_source_rgb cr 1. 1. 1.;
          paint cr;
          set_source_rgb cr 0. 0. 0.;
          draw_score cr page.content;
          Cairo.Surface.show_page surface)
        pages;
      Cairo.Surface.finish surface
