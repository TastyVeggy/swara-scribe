let () =
  let args = Array.to_list Sys.argv in
  match args with
  | [ __prog ] -> Printf.printf "Please provide a file\n"
  | [ __prog; filename ] -> Swara_scribe.Engraver.engrave filename
  | _ -> Printf.printf "Usage: srgm <file>\n"
