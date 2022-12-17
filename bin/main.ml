open Core

let read_input path day =
  let filename = Filename.concat path (Printf.sprintf "day%02d.txt" day) in
  try Ok (In_channel.read_all filename) with
  | exn -> Error (Base.Error.of_exn exn)
;;

let run path day =
  match
    let open Or_error.Let_syntax in
    read_input path day >>= Solutions.solve day
  with
  | Ok solution -> print_endline solution
  | Error err ->
    Printf.fprintf Out_channel.stderr "error: %s" (Base.Error.to_string_hum err)
;;

let command =
  Command.basic
    ~summary:"Run AOC2022 puzzle solvers"
    ~readme:(fun () -> "Runs solvers for Advent of Code 2022 puzzles for specified day.")
    (let%map_open.Command path =
       flag "-i" (optional_with_default "resources/" string) ~doc:"path to puzzle inputs."
     and day = anon ("day" %: int) in
     fun () -> run path day)
;;

let () = Command_unix.run command
