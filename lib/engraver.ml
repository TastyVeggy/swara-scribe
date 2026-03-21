let ( let* ) = Result.bind

type error = Lex_error of Lexer.Error.t | Parse_error of Parser.Error.t

let process (filename : string) (main_instrument : string option) :
    (unit, error) result =
  let input = In_channel.with_open_text filename In_channel.input_all in
  let* tokens = Lexer.lex input |> Result.map_error (fun e -> Lex_error e) in
  let* score =
    Parser.parse tokens |> Result.map_error (fun e -> Parse_error e)
  in
  let layout = Layout.layout score main_instrument in
  Render.generate_score_pdf
    (Printf.sprintf "%s.pdf" (Filename.remove_extension filename))
    layout;
  Ok ()

let engrave (filename : string) (main_instrument : string option) : unit =
  let message =
    match process filename main_instrument with
    | Ok () -> "Successfully rendered"
    | Error (Lex_error e) -> Format.asprintf "%a" Lexer.Error.pp e
    | Error (Parse_error e) -> Format.asprintf "%a" Parser.Error.pp e
  in
  Printf.printf "%s\n" message
