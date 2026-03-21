let () =
  let instrument = ref None in
  let files = ref [] in
  let command_usage = "Usage: scribe [--main-instrument <name>] <file>" in
  let arg_specs =
    ref
      [
        ( "--main-instrument",
          Arg.String (fun s -> instrument := Some s),
          "<name>  Highlight this instrument part (others rendered in grey)" );
      ]
  in
  let anon_fun f = files := f :: !files in
  let current = ref 0 in
  let argv = ref Sys.argv in
  Arg.parse_and_expand_argv_dynamic current argv arg_specs anon_fun
    command_usage;
  match List.rev !files with
  | [] -> Printf.printf "Please provide a file\n%s\n" command_usage
  | [ filename ] -> Swara_scribe.Engraver.engrave filename !instrument
  | _ -> Printf.printf "%s\n" command_usage
