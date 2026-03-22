let () =
  let instrument = ref None in
  let show_bar_no = ref false in
  let title = ref None in
  let font = ref "Noto Sans Mono" in
  let files = ref [] in

  let command_usage =
    "Usage: scribe [--main-instrument <name>] [--show-bar-no] <file>"
  in

  let arg_specs =
    ref
      [
        ( "--main-instrument",
          Arg.String (fun s -> instrument := Some s),
          " <name>  Highlight this instrument part (others rendered in grey)" );
        ( "--show-bar-no",
          Arg.Set show_bar_no,
          " Show bar numbers (default: off)" );
        ("--font", Arg.String (fun s -> font := s), "<name> Font for score");
        ( "--title",
          Arg.String (fun s -> title := Some s),
          "<name> Title of the piece" );
      ]
  in

  let anon_fun f = files := f :: !files in

  let current = ref 0 in
  let argv = ref Sys.argv in

  try
    Arg.parse_and_expand_argv_dynamic current argv arg_specs anon_fun
      command_usage;

    let config : Swara_scribe.Config.config =
      {
        main_instrument = !instrument;
        show_bar_no = !show_bar_no;
        title = !title;
        font = !font;
      }
    in

    match List.rev !files with
    | [] ->
        Printf.eprintf "Error: Please provide a file\n%s\n%!" command_usage;
        exit 1
    | [ filename ] -> Swara_scribe.Engraver.engrave filename config
    | _ ->
        Printf.eprintf "Error: Too many files provided\n%s\n%!" command_usage;
        exit 1
  with
  | Arg.Bad msg ->
      Printf.eprintf "Error: %s\n%!" msg;
      exit 1
  | Arg.Help msg ->
      Printf.printf "%s\n%!" msg;
      exit 0
