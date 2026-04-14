let ( let* ) = Result.bind

type error =
  | Lex_error of Lexer.Error.t
  | Parse_error of Parse_interface.Error.t
  | Score_error of Score.Error.t

let pp_error fmt = function
  | Lex_error e -> Format.fprintf fmt "%a" Lexer.Error.pp e
  | Parse_error e -> Format.fprintf fmt "%a" Parse_interface.Error.pp e
  | Score_error e -> Format.fprintf fmt "%a" Score.Error.pp e

let process (filename : string) (config : Config.config) : (unit, error) result
    =
  let input = In_channel.with_open_text filename In_channel.input_all in
  let* tokens = Lexer.lex input |> Result.map_error (fun e -> Lex_error e) in
  let* ast =
    Parse_interface.parse tokens |> Result.map_error (fun e -> Parse_error e)
  in
  let* score = Score.of_ast ast |> Result.map_error (fun e -> Score_error e) in
  let layout = Layout.layout score config in
  Render.generate_score_pdf
    (Printf.sprintf "%s.pdf" (Filename.remove_extension filename))
    layout config;
  Ok ()

let engrave (filename : string) (config : Config.config) : unit =
  match process filename config with
  | Ok () -> Printf.printf "Successfully rendered\n"
  | Error e -> Format.printf "%a\n" pp_error e
