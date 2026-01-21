open Lexer
open Ir

let ( let* ) = Result.bind

module Error = struct
  type t =
    | Unexpected_token of Token.t
    | Empty_input
    | Matra_Count_Inconsistency_Across_Parts
    | Barline_Mismatch_Across_Parts
    | Length_Mismatch_Across_Parts
    | Invalid_Structural_Alignment
    | Inconsistent_Instrumentation_Within_Part

  let pp fmt e =
    let add_header detail = Format.fprintf fmt "Parsing error: %s" detail in

    match e with
    | Unexpected_token token ->
        add_header (Format.asprintf "Unable to parse token '%a'" Token.pp token)
    | Empty_input -> add_header "Nothing to parse"
    | Matra_Count_Inconsistency_Across_Parts ->
        add_header
          "Different parts of the same line should have the same number of \
           matras"
    | Length_Mismatch_Across_Parts ->
        add_header "There is a length mistmatch across the parts"
    | Barline_Mismatch_Across_Parts ->
        add_header "There is a barline mismatch between the parts"
    | Invalid_Structural_Alignment ->
        add_header "Structural misalignment between the parts"
    | Inconsistent_Instrumentation_Within_Part ->
        add_header "Inconsistent instrumentation within part"
end

let parse_symbol : Token.t list -> (Symbol.t * Token.t list, Error.t) result =
  let open Token in
  let open Symbol in
  function
  | LETTER c :: rest ->
      let octave, rest' =
        match rest with
        | APOSTROPHE :: tail -> (Some Higher_octave, tail)
        | COMMA :: tail -> (Some Lower_octave, tail)
        | _ -> (None, rest)
      in
      Ok (Swaram (c, octave), rest')
  | DASH :: rest -> Ok (Sustain, rest)
  | FULL_STOP :: rest -> Ok (Rest, rest)
  | token :: _ -> Error (Error.Unexpected_token token)
  | [] -> Error Error.Empty_input

let parse_matra_part (ts : Token.t list) :
    (Symbol.t list * Token.t list, Error.t) result =
  let open Token in
  let rec parse_matra_part_ acc ts =
    match ts with
    | [] -> Ok (List.rev acc, [])
    | WHITESPACE :: _ -> Ok (List.rev acc, ts)
    | NEWLINE :: _ -> Ok (List.rev acc, ts)
    | EOF :: _ -> Ok (List.rev acc, ts)
    | ts ->
        let* symbol, rest = parse_symbol ts in
        parse_matra_part_ (symbol :: acc) rest
  in
  parse_matra_part_ [] ts

let extract_line (raw_score : Cst.t) : (Elab.line * Cst.t, Error.t) result =
  let open Cst in
  let rec extract_line_ (parts : Elab.line) (curr_part : Elab.part)
      (curr_part_instrument : string option option) (curr_matra_count : int)
      (expected_matra_count : int option) :
      Cst.t -> (Elab.line * Cst.t, Error.t) result =
    let check_alignment () =
      match expected_matra_count with
      | None -> Ok curr_matra_count
      | Some expected ->
          if curr_matra_count = expected then Ok expected
          else Error Error.Matra_Count_Inconsistency_Across_Parts
    in
    function
    | [] ->
        check_alignment ()
        |> Result.map (fun _ -> (List.rev (List.rev curr_part :: parts), []))
    | Newline :: rest ->
        check_alignment ()
        |> Result.map (fun _ -> (List.rev (List.rev curr_part :: parts), rest))
    | Barline :: rest ->
        extract_line_ parts (Barline :: curr_part) curr_part_instrument
          curr_matra_count expected_matra_count rest
    | Matra_Part matra_part :: rest ->
        let* instrument =
          match curr_part_instrument with
          | None -> Ok (Some matra_part.instrument)
          | Some inst ->
              if inst = matra_part.instrument then Ok (Some inst)
              else Error Error.Inconsistent_Instrumentation_Within_Part
        in
        extract_line_ parts
          (Matra_Part matra_part :: curr_part)
          instrument (curr_matra_count + 1) expected_matra_count rest
    | Next_Part :: rest ->
        let* expected = check_alignment () |> Result.map (fun c -> Some c) in
        extract_line_ (List.rev curr_part :: parts) [] None 0 expected rest
  in

  extract_line_ [] [] None 0 None raw_score

let synchronise_line (line : Elab.line) (starting_bar_no : int) :
    (Ast.line, Error.t) result =
  let rec synchronise_line_ acc no_of_bars heads_and_tails =
    if List.for_all (function [] -> true | _ -> false) heads_and_tails then
      Ok (List.rev acc, no_of_bars)
    else if List.exists (function [] -> true | _ -> false) heads_and_tails
    then Error Error.Length_Mismatch_Across_Parts
    else
      let current_column = List.map List.hd heads_and_tails in
      let next_tails = List.map List.tl heads_and_tails in

      let has_barline = List.exists (fun x -> x = Cst.Barline) current_column in
      let all_barline =
        List.for_all (fun x -> x = Cst.Barline) current_column
      in

      if has_barline then
        if all_barline then
          synchronise_line_ (Ast.Barline :: acc) (no_of_bars + 1) next_tails
        else Error Error.Barline_Mismatch_Across_Parts
      else
        let extract_matra = function
          | Cst.Matra_Part p -> Ok p
          | _ -> Error Error.Invalid_Structural_Alignment
        in
        let* matra =
          Util.sequence_results (List.map extract_matra current_column)
        in
        synchronise_line_ (Ast.Matra matra :: acc) no_of_bars next_tails
  in
  let* content, no_of_bars = synchronise_line_ [] 1 line in
  Ok
    {
      Ast.no_of_parts = List.length line;
      Ast.content;
      Ast.starting_bar_no;
      Ast.no_of_bars;
    }

let remove_trailing_newlines_nextparts (score : Cst.t) : Cst.t =
  let open Cst in
  let is_trailing = function Newline | Next_Part -> true | _ -> false in
  let rec drop_while pred = function
    | [] -> []
    | x :: xs when pred x -> drop_while pred xs
    | l -> l
  in
  score |> List.rev |> drop_while is_trailing |> List.rev

let synchronise (raw_score : Cst.t) (instrumentation : string list) :
    (Ast.t, Error.t) result =
  let trimmed_score = remove_trailing_newlines_nextparts raw_score in

  let rec synchronise_ acc starting_bar_no current_score =
    match current_score with
    | [] -> Ok { Ast.content = List.rev acc; Ast.instrumentation }
    | _ ->
        let* line, rest = extract_line current_score in
        let* synchronised_line = synchronise_line line starting_bar_no in
        synchronise_ (synchronised_line :: acc)
          (synchronised_line.starting_bar_no + synchronised_line.no_of_bars)
          rest
  in
  synchronise_ [] 1 trimmed_score

module InstrumentSet = Set.Make (String)

let parse : Token.t list -> (Ast.t, Error.t) result =
  let open Token in
  let rec parse_ acc inst_set curr_inst =
    let open Cst in
    function
    | [] | EOF :: _ -> Ok (List.rev acc, inst_set)
    | INSTRUMENT_TEXT inst :: rest ->
        parse_ acc (InstrumentSet.add inst inst_set) (Some inst) rest
    | NEWLINE :: NEWLINE :: rest -> parse_ (Newline :: acc) inst_set None rest
    | NEWLINE :: rest -> parse_ (Next_Part :: acc) inst_set None rest
    | BARLINE :: rest -> parse_ (Barline :: acc) inst_set curr_inst rest
    | WHITESPACE :: rest -> parse_ acc inst_set curr_inst rest
    | LYRICS t :: rest ->
        parse_
          (Matra_Part { symbols = [ Symbol.Lyrics t ]; instrument = curr_inst }
          :: acc)
          inst_set curr_inst rest
    | ts ->
        let* symbols, rest' = parse_matra_part ts in
        parse_
          (Matra_Part { symbols; instrument = curr_inst } :: acc)
          inst_set curr_inst rest'
  in
  function
  | [] -> Error Error.Empty_input
  | ts ->
      let* raw_score, inst_set = parse_ [] InstrumentSet.empty None ts in
      let* score = synchronise raw_score (InstrumentSet.to_list inst_set) in
      Ok score
