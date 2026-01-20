let rec sequence_results : ('a, 'e) result list -> ('a list, 'e) result =
  let ( let* ) = Result.bind in
  function
  | [] -> Ok []
  | Error e :: _ -> Error e
  | Ok h :: t ->
      let* rest = sequence_results t in
      Ok (h :: rest)
