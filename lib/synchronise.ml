open Ir

let ( let* ) = Result.bind

module Elab = struct
  type part = Ast.element list
  type line = part list
end

module Error = struct
  type t =
    | Matra_Count_Inconsistency of {
        part_index : int;
        expected : int;
        got : int;
      }
    | Barline_Mismatch of { column_index : int; part_had_barline : bool list }
    | Length_Mismatch of { part_index : int; exhausted : bool list }
    | Invalid_Structural_Alignment of { column_index : int; got : Ast.element }
    | Inconsistent_Instrumentation of {
        part_index : int;
        first_seen : string option;
        conflicting : string option;
      }

  let pp fmt = function
    | Matra_Count_Inconsistency { part_index; expected; got } ->
        Format.fprintf fmt
          "Synchronise error: part %d has %d matras but expected %d" part_index
          got expected
    | Barline_Mismatch { column_index; part_had_barline } ->
        let part_strs =
          List.mapi
            (fun i had ->
              Printf.sprintf "part %d: %s" i
                (if had then "barline" else "no barline"))
            part_had_barline
          |> String.concat ", "
        in
        Format.fprintf fmt
          "Synchronise error: barline mismatch at column %d — %s" column_index
          part_strs
    | Length_Mismatch { part_index = _; exhausted } ->
        let part_strs =
          List.mapi
            (fun i ex ->
              Printf.sprintf "part %d: %s" i
                (if ex then "exhausted" else "has elements"))
            exhausted
          |> String.concat ", "
        in
        Format.fprintf fmt
          "Synchronise error: length mismatch across parts — %s" part_strs
    | Invalid_Structural_Alignment { column_index; got } ->
        Format.fprintf fmt
          "Synchronise error: expected matra at column %d but got %a"
          column_index Ast.pp_element got
    | Inconsistent_Instrumentation { part_index; first_seen; conflicting } ->
        let pp_inst = function
          | None -> "none"
          | Some s -> Printf.sprintf "\"%s\"" s
        in
        Format.fprintf fmt
          "Synchronise error: part %d has inconsistent instrumentation — first \
           saw %s, then saw %s"
          part_index (pp_inst first_seen) (pp_inst conflicting)
end

module InstrumentSet = Set.Make (String)

let stamp_instruments (ast : Ast.t) : Ast.t * string list =
  let rec stamp_instruments_ acc inst_set curr = function
    | [] -> (List.rev acc, InstrumentSet.to_list inst_set)
    | Ast.Instrument i :: rest ->
        stamp_instruments_ acc (InstrumentSet.add i inst_set) (Some i) rest
    | Ast.Matra_Part mp :: rest ->
        stamp_instruments_
          (Ast.Matra_Part { mp with instrument = curr } :: acc)
          inst_set curr rest
    | Ast.Newline :: rest ->
        stamp_instruments_ (Ast.Newline :: acc) inst_set None rest
    | other :: rest -> stamp_instruments_ (other :: acc) inst_set curr rest
  in
  stamp_instruments_ [] InstrumentSet.empty None ast

let extract_line (ast : Ast.t) : (Elab.line * Ast.t, Error.t) result =
  let rec extract_line_ parts curr_part curr_inst curr_count expected part_index
      = function
    | [] -> (
        let got = curr_count in
        match expected with
        | Some exp when got <> exp ->
            Error
              (Error.Matra_Count_Inconsistency
                 { part_index; expected = exp; got })
        | _ -> Ok (List.rev (List.rev curr_part :: parts), []))
    | Ast.Newline :: rest -> (
        let got = curr_count in
        match expected with
        | Some exp when got <> exp ->
            Error
              (Error.Matra_Count_Inconsistency
                 { part_index; expected = exp; got })
        | _ -> Ok (List.rev (List.rev curr_part :: parts), rest))
    | Ast.Barline :: rest ->
        extract_line_ parts (Ast.Barline :: curr_part) curr_inst curr_count
          expected part_index rest
    | Ast.Matra_Part mp :: rest ->
        let* inst =
          match curr_inst with
          | None -> Ok mp.instrument
          | Some i when Some i = mp.instrument -> Ok (Some i)
          | Some first_seen ->
              Error
                (Error.Inconsistent_Instrumentation
                   {
                     part_index;
                     first_seen = Some first_seen;
                     conflicting = mp.instrument;
                   })
        in
        extract_line_ parts
          (Ast.Matra_Part mp :: curr_part)
          inst (curr_count + 1) expected part_index rest
    | Ast.Next_Part :: rest ->
        let got = curr_count in
        let* expected =
          match expected with
          | Some exp when got <> exp ->
              Error
                (Error.Matra_Count_Inconsistency
                   { part_index; expected = exp; got })
          | _ -> Ok (Some got)
        in
        extract_line_
          (List.rev curr_part :: parts)
          [] None 0 expected (part_index + 1) rest
    | Ast.Instrument _ :: rest ->
        extract_line_ parts curr_part curr_inst curr_count expected part_index
          rest
  in
  extract_line_ [] [] None 0 None 0 ast

let synchronise_line (line : Elab.line) (starting_bar_no : int) :
    (Score.line, Error.t) result =
  let rec synchronise_line_ acc no_of_bars col_index heads_and_tails =
    let exhausted =
      List.map (function [] -> true | _ -> false) heads_and_tails
    in
    if List.for_all (fun x -> x) exhausted then Ok (List.rev acc, no_of_bars)
    else if List.exists (fun x -> x) exhausted then
      Error
        (Error.Length_Mismatch
           {
             part_index =
               (let i = ref 0 in
                List.iteri (fun idx ex -> if ex then i := idx) exhausted;
                !i);
             exhausted;
           })
    else
      let current_column = List.map List.hd heads_and_tails in
      let next_tails = List.map List.tl heads_and_tails in
      let barline_flags = List.map (fun x -> x = Ast.Barline) current_column in
      let has_barline = List.exists (fun x -> x) barline_flags in
      let all_barline = List.for_all (fun x -> x) barline_flags in
      if has_barline then
        if all_barline then
          synchronise_line_ (Score.Barline :: acc) (no_of_bars + 1)
            (col_index + 1) next_tails
        else
          Error
            (Error.Barline_Mismatch
               { column_index = col_index; part_had_barline = barline_flags })
      else
        let extract_matra col_idx = function
          | Ast.Matra_Part p -> Ok p
          | other ->
              Error
                (Error.Invalid_Structural_Alignment
                   { column_index = col_idx; got = other })
        in
        let* matras =
          Util.sequence_results
            (List.map (extract_matra col_index) current_column)
        in
        synchronise_line_
          (Score.Matra matras :: acc)
          no_of_bars (col_index + 1) next_tails
  in
  let* content, no_of_bars = synchronise_line_ [] 1 0 line in
  Ok
    {
      Score.no_of_parts = List.length line;
      Score.content;
      Score.starting_bar_no;
      Score.no_of_bars;
    }

let remove_trailing (ast : Ast.t) : Ast.t =
  let is_trailing = function
    | Ast.Newline | Ast.Next_Part -> true
    | _ -> false
  in
  let rec drop pred = function
    | [] -> []
    | x :: xs when pred x -> drop pred xs
    | l -> l
  in
  ast |> List.rev |> drop is_trailing |> List.rev

let synchronise (raw_ast : Ast.t) : (Score.t, Error.t) result =
  let stamped, instrumentation = stamp_instruments raw_ast in
  let trimmed = remove_trailing stamped in
  let rec synchronise_line_ acc bar_no ast =
    match ast with
    | [] -> Ok { Score.content = List.rev acc; Score.instrumentation }
    | _ ->
        let* line, rest = extract_line ast in
        let* sync_line = synchronise_line line bar_no in
        synchronise_line_ (sync_line :: acc)
          (sync_line.Score.starting_bar_no + sync_line.Score.no_of_bars)
          rest
  in
  synchronise_line_ [] 1 trimmed
