open Ir

let unit_width = 1240.
let unit_height = unit_width *. Float.sqrt 2.
let scale = 1.
let scale_y = scale *. unit_height
let scale_x = scale *. unit_width

(* margin *)
let left_margin_x = 0.07 *. scale_x
let right_margin_x = 0.09 *. scale_x
let top_margin_y = 0.1 *. scale_y
let bottom_margin_y = 0.05 *. scale_y

(* sizing *)
let text_size = 0.03 *. scale_y
let text_width = 0.6 *. text_size

(* horizontal spacing *)
let matra_x_padding = 0.06 *. scale_x
let barline_x_padding = 0.04 *. scale_x
let instrument_x_padding = 0.04 *. scale_x
let note_width = 0.033 *. scale_x
let barno_x_offset_from_line_start = 0. *. scale_x

(* vertical spacing *)
let barline_height = 0.04 *. scale_y
let line_y_padding = 2.0 *. barline_height
let part_y_padding = 0.5 *. barline_height
let barno_y_offset_from_line_centre = 0.9 *. barline_height

type position = Layout_Tree.position

let part_emphasised (main_instrument : string option)
    (instrument : string option) : bool =
  match main_instrument with
  | None -> true
  | Some h -> (
      match instrument with Some i -> String.equal h i | None -> false)

(* extract the ordered list of instrument names from the first Matra in the line. Assumes ast is such taht every matra in a line has the same parts in the same order, so the first one is representative*)
let line_part_instruments (content : Ast.element list) : string option list =
  match List.find_opt (function Ast.Matra _ -> true | _ -> false) content with
  | Some (Ast.Matra matra) ->
      List.map (fun (mp : Ir.matra_part) -> mp.instrument) matra
  | _ -> []

let layout_instrumentation (pos : position) (elems : Ast.element list)
    (main_instrument : string option) : Layout_Tree.element list =
  let rec layout_instrumentation_ = function
    | [] -> []
    | Ast.Barline :: rest -> layout_instrumentation_ rest
    | Ast.Matra matra :: _ ->
        let rec loop acc inner_y = function
          | [] -> List.rev acc
          | (matra_part : Ir.matra_part) :: rest ->
              let text =
                match matra_part.instrument with None -> "" | Some s -> s
              in
              let is_emphasised =
                part_emphasised main_instrument matra_part.instrument
              in
              loop
                (Layout_Tree.LInstrument
                   {
                     text;
                     right = { x = pos.x -. instrument_x_padding; y = inner_y };
                     is_emphasised;
                   }
                :: acc)
                (inner_y +. barline_height +. part_y_padding)
                rest
        in
        loop [] pos.y matra
  in
  layout_instrumentation_ elems

let layout_bar_number (pos : position) (bar_no : int) : Layout_Tree.element =
  let text = string_of_int bar_no in
  let y = pos.y -. barno_y_offset_from_line_centre in
  let x = pos.x -. barno_x_offset_from_line_start in
  Layout_Tree.LBarno { text; centre = { x; y } }

type layout_result = {
  elements : Layout_Tree.element list;
  width : float;
  height : float;
}

let layout_matra_part (pos : position) (matra_width : float)
    (matra_part : matra_part) (is_emphasised : bool) : Layout_Tree.element list
    =
  let rec layout_matra_part_ acc inner_x = function
    | [] -> List.rev acc
    | c :: rest ->
        let elem =
          Layout_Tree.LSymbol
            { symbol = c; centre = { pos with x = inner_x }; is_emphasised }
        in
        layout_matra_part_ (elem :: acc) (inner_x +. note_width) rest
  in
  let current_part_width =
    float_of_int (List.length matra_part.symbols) *. note_width
  in
  let start_x = pos.x +. (matra_width /. 2.) -. (current_part_width /. 2.) in
  layout_matra_part_ [] start_x matra_part.symbols

let layout_element (pos : position) (part_instruments : string option list)
    (main_instrument : string option) : Ast.element -> layout_result = function
  | Ast.Barline ->
      let rec layout_barlines acc inner_y = function
        | [] -> (List.rev acc, inner_y)
        | instrument :: rest ->
            let is_emphasised = part_emphasised main_instrument instrument in
            layout_barlines
              (Layout_Tree.LBarline
                 {
                   height = barline_height;
                   centre = { pos with y = inner_y };
                   is_emphasised;
                 }
              :: acc)
              (inner_y +. barline_height +. part_y_padding)
              rest
      in
      let elems, height = layout_barlines [] pos.y part_instruments in
      { elements = elems; width = 0.; height }
  | Ast.Matra matra ->
      let calc_width ~offset = function
        | [ Symbol.Lyrics s ] ->
            float_of_int (max (String.length s + offset) 0) *. text_width
        | symbols ->
            float_of_int (max (List.length symbols + offset) 0) *. note_width
      in
      let matra_width =
        List.fold_left
          (fun acc (mp : Ir.matra_part) ->
            max acc (calc_width ~offset:0 mp.symbols))
          0. matra
      in
      let advance_width =
        List.fold_left
          (fun acc (mp : Ir.matra_part) ->
            max acc (calc_width ~offset:(-1) mp.symbols))
          0. matra
      in

      let rec layout_matra acc inner_y = function
        | [] -> (List.rev acc, inner_y)
        | matra_part :: rest ->
            let is_emphasised =
              part_emphasised main_instrument matra_part.instrument
            in
            let layout_elems =
              layout_matra_part { pos with y = inner_y } matra_width matra_part
                is_emphasised
            in
            layout_matra (layout_elems @ acc)
              (inner_y +. barline_height +. part_y_padding)
              rest
      in
      let elems, height = layout_matra [] pos.y matra in
      { elements = elems; width = advance_width; height }

let find_x_padding (left : Ast.element) (right : Ast.element) : float =
  if left = Ast.Barline || right = Ast.Barline then barline_x_padding
  else matra_x_padding

let layout_line (pos : position) (line : Ast.line)
    (main_instrument : string option) =
  let bar_no_elem = layout_bar_number pos line.starting_bar_no in
  let instrument_elems =
    layout_instrumentation pos line.content main_instrument
  in
  let part_instruments = line_part_instruments line.content in

  let rec layout_line_ (acc : layout_result) inner_x = function
    | [] -> { acc with elements = List.rev acc.elements }
    | left :: rest -> (
        let layout_res =
          layout_element { pos with x = inner_x } part_instruments
            main_instrument left
        in
        match rest with
        | [] ->
            {
              elements = List.rev (layout_res.elements @ acc.elements);
              width = max acc.width (inner_x +. layout_res.width);
              height = max acc.height layout_res.height;
            }
        | right :: _ ->
            let x_next =
              inner_x +. layout_res.width +. find_x_padding left right
            in
            layout_line_
              { acc with elements = layout_res.elements @ acc.elements }
              x_next rest)
  in
  let line_layout =
    layout_line_ { elements = []; width = 0.; height = 0. } pos.x line.content
  in
  {
    line_layout with
    elements = bar_no_elem :: (instrument_elems @ line_layout.elements);
  }

let line_block_height (no_of_parts : int) : float =
  (float_of_int no_of_parts *. barline_height)
  +. (float_of_int (no_of_parts - 1) *. part_y_padding)

let shift_elements_y (dy : float) (elems : Layout_Tree.element list) :
    Layout_Tree.element list =
  let sp (p : Layout_Tree.position) = { p with y = p.y +. dy } in
  List.map
    (function
      | Layout_Tree.LSymbol s ->
          Layout_Tree.LSymbol { s with centre = sp s.centre }
      | Layout_Tree.LBarline b ->
          Layout_Tree.LBarline { b with centre = sp b.centre }
      | Layout_Tree.LInstrument i ->
          Layout_Tree.LInstrument { i with right = sp i.right }
      | Layout_Tree.LBarno n ->
          Layout_Tree.LBarno { n with centre = sp n.centre })
    elems

type pre_line = {
  pl_elements : Layout_Tree.element list;
  pl_width : float;
  pl_block_h : float;
}

type pager = {
  done_pages : Layout_Tree.t;
  cur_elements : Layout_Tree.element list; (*reversed order*)
  cur_width : float;
  cur_y : float;
}

let make_pager () : pager =
  { done_pages = []; cur_elements = []; cur_width = 0.; cur_y = top_margin_y }

let close_page (page_width : float) (page_height : float) (p : pager) : pager =
  let page : Layout_Tree.page =
    {
      content = List.rev p.cur_elements;
      width = page_width;
      height = page_height;
    }
  in
  {
    done_pages = page :: p.done_pages;
    cur_elements = [];
    cur_width = 0.;
    cur_y = top_margin_y;
  }

let finish_pager (page_width : float) (page_height : float) (p : pager) :
    Layout_Tree.t =
  let last : Layout_Tree.page =
    {
      width = page_width;
      height = page_height;
      content = List.rev p.cur_elements;
    }
  in
  List.rev (last :: p.done_pages)

let layout (score : Ast.t) (main_instrument : string option) : Layout_Tree.t =
  let longest_inst =
    List.fold_left
      (fun acc inst -> max acc (String.length inst))
      0 score.instrumentation
  in
  let inst_margin =
    if longest_inst = 0 then 0.
    else (float_of_int longest_inst *. text_width) +. instrument_x_padding
  in
  match score.content with
  | [] ->
      [
        {
          Layout_Tree.width = left_margin_x +. right_margin_x;
          height = (left_margin_x +. right_margin_x) *. Float.sqrt 2.;
          content = [];
        };
      ]
  | _ ->
      (*lay out every line at top_margin_y to find max width*)
      let dummy_pos =
        { Layout_Tree.x = left_margin_x +. inst_margin; y = top_margin_y }
      in
      let pre_lines =
        List.map
          (fun (line : Ast.line) ->
            let lr = layout_line dummy_pos line main_instrument in
            {
              pl_elements = lr.elements;
              pl_width = lr.width;
              pl_block_h = line_block_height line.no_of_parts;
            })
          score.content
      in
      let max_content_w =
        List.fold_left (fun acc pl -> max acc pl.pl_width) 0. pre_lines
      in
      let page_width = max_content_w +. right_margin_x in
      let page_height = page_width *. Float.sqrt 2. in

      (* Distribution of lines across pages *)
      let rec go (p : pager) = function
        | [] -> finish_pager page_width page_height p
        | (pl : pre_line) :: rest ->
            let p =
              if
                p.cur_y +. pl.pl_block_h +. bottom_margin_y > page_height
                && p.cur_y > top_margin_y
              then close_page page_width page_height p
              else p
            in
            let dy = p.cur_y -. top_margin_y in
            let elems =
              if dy = 0. then pl.pl_elements
              else shift_elements_y dy pl.pl_elements
            in
            go
              {
                p with
                cur_elements = elems @ p.cur_elements;
                cur_width = max pl.pl_width p.cur_width;
                cur_y = p.cur_y +. pl.pl_block_h +. line_y_padding;
              }
              rest
      in
      go (make_pager ()) pre_lines
