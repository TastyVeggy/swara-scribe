open Ir

let unit_width = 1240.
let unit_height = 1754.
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
let text_width = 0.6 *. text_size (* courier monospace *)

(* horizontal spacing *)
let matra_x_padding = 0.06 *. scale_x
let barline_x_padding = 0.04 *. scale_x
let instrument_x_padding = 0.04 *. scale_x
let note_width = 0.033 *. scale_x
let barno_x_offset_from_line_start = 0. *. scale_x

(* vertical spacing *)
let barline_height = 0.04 *. scale_y
let line_y_padding = 1.5 *. barline_height
let part_y_padding = 0.5 *. barline_height
let barno_y_offset_from_line_centre = 0.9 *. barline_height

type position = Layout_Tree.position

let layout_instrumentation (pos : position) (elems : Ast.element list) :
    Layout_Tree.element list =
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
              loop
                (Layout_Tree.LInstrument
                   {
                     text;
                     right = { x = pos.x -. instrument_x_padding; y = inner_y };
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
    (matra_part : matra_part) : Layout_Tree.element list =
  let rec layout_matra_part_ acc inner_x = function
    | [] -> List.rev acc
    | c :: rest ->
        let elem =
          Layout_Tree.LSymbol { symbol = c; centre = { pos with x = inner_x } }
        in
        layout_matra_part_ (elem :: acc) (inner_x +. note_width) rest
  in
  let current_part_width =
    float_of_int (List.length matra_part.symbols) *. note_width
  in
  let start_x = pos.x +. (matra_width /. 2.) -. (current_part_width /. 2.) in
  layout_matra_part_ [] start_x matra_part.symbols

let layout_element (pos : position) (no_of_parts : int) :
    Ast.element -> layout_result = function
  | Ast.Barline ->
      let rec layout_barlines acc inner_y i =
        match i with
        | 0 -> (List.rev acc, inner_y)
        | _ ->
            layout_barlines
              (Layout_Tree.LBarline
                 { height = barline_height; centre = { pos with y = inner_y } }
              :: acc)
              (inner_y +. barline_height +. part_y_padding)
              (i - 1)
      in
      let elems, height = layout_barlines [] pos.y no_of_parts in
      { elements = elems; width = 0.; height }
  | Ast.Matra matra ->
      let max_symbols =
        List.fold_left
          (fun acc (mp : Ir.matra_part) -> max acc (List.length mp.symbols))
          0 matra
      in
      let matra_width = float_of_int max_symbols *. note_width in

      let rec layout_matra acc inner_y = function
        | [] -> (List.rev acc, inner_y)
        | matra_part :: rest ->
            let layout_elems =
              layout_matra_part { pos with y = inner_y } matra_width matra_part
            in
            layout_matra (layout_elems @ acc)
              (inner_y +. barline_height +. part_y_padding)
              rest
      in
      let elems, height = layout_matra [] pos.y matra in
      {
        elements = elems;
        width = max (float_of_int (max_symbols - 1)) 0. *. note_width;
        height;
      }

let find_x_padding (left : Ast.element) (right : Ast.element) : float =
  if left = Ast.Barline || right = Ast.Barline then barline_x_padding
  else matra_x_padding

let layout_line (pos : position) (line : Ast.line) =
  let bar_no_elem = layout_bar_number pos line.starting_bar_no in
  let instrument_elems = layout_instrumentation pos line.content in

  let rec layout_line_ (acc : layout_result) inner_x = function
    | [] -> { acc with elements = List.rev acc.elements }
    | left :: rest -> (
        let layout_res =
          layout_element { pos with x = inner_x } line.no_of_parts left
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

let layout (score : Ast.t) : Layout_Tree.t =
  let longest_inst =
    List.fold_left
      (fun acc inst -> max acc (String.length inst))
      0 score.instrumentation
  in
  let inst_margin =
    if longest_inst = 0 then 0.
    else (float_of_int longest_inst *. text_width) +. instrument_x_padding
  in
  let rec layout_ acc pos = function
    | [] -> { acc with elements = List.rev acc.elements }
    | (line : Ast.line) :: rest ->
        let layout_res = layout_line pos line in
        let y_next =
          pos.y
          +. (float_of_int line.no_of_parts *. barline_height)
          +. (float_of_int (line.no_of_parts - 1) *. part_y_padding)
          +. line_y_padding
        in
        layout_
          {
            elements = layout_res.elements @ acc.elements;
            width = max layout_res.width acc.width;
            height = max layout_res.height acc.height;
          }
          { pos with y = y_next } rest
  in
  let layout_res =
    layout_
      { elements = []; width = left_margin_x; height = top_margin_y }
      { x = left_margin_x +. inst_margin; y = top_margin_y }
      score.content
  in
  {
    content = layout_res.elements;
    width = layout_res.width +. right_margin_x;
    height = layout_res.height +. bottom_margin_y;
  }
