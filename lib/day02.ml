open Base

module Solver : Solver.S = struct
  let day = 2

  type rps =
    | Rock
    | Paper
    | Scissors
  [@@deriving sexp]

  let parse_rps = function
    | 'A' | 'X' -> Rock
    | 'B' | 'Y' -> Paper
    | 'C' | 'Z' -> Scissors
    | c -> failwith @@ Printf.sprintf "unparsable char %c" c
  ;;

  type input = (rps * rps) list [@@deriving sexp]

  let parse s =
    let ls = String.split_lines s in
    try
      Ok
        (List.map ~f:(fun s -> parse_rps (String.get s 0), parse_rps (String.get s 2)) ls)
    with
    | exn -> Error (Error.of_exn ~backtrace:`Get exn)
  ;;

  let outcome_score = function
    | Scissors, Rock | Paper, Scissors | Rock, Paper -> 6 (* won *)
    | Rock, Scissors | Scissors, Paper | Paper, Rock -> 0 (* lost *)
    | _ -> 3 (* draw *)
  ;;

  let outcome_shape = function
    | Rock -> 1
    | Paper -> 2
    | Scissors -> 3
  ;;

  let outcome pair = outcome_score pair + outcome_shape (snd pair)
  let total_score ~f input = List.fold ~init:0 ~f:( + ) @@ List.map ~f input
  let solve1 input = Ok (Int.to_string @@ total_score ~f:outcome input)

  let transform_pair = function
    | Rock, Rock (* lose *) -> Rock, Scissors
    | Rock, Paper (* draw *) -> Rock, Rock
    | Rock, Scissors (* win *) -> Rock, Paper
    | Scissors, Rock (* lose *) -> Scissors, Paper
    | Scissors, Paper (* draw *) -> Scissors, Scissors
    | Scissors, Scissors (* win *) -> Scissors, Rock
    | Paper, Rock (* lose *) -> Paper, Rock
    | Paper, Paper (* draw *) -> Paper, Paper
    | Paper, Scissors (* win *) -> Paper, Scissors
  ;;

  let outcome2 pair =
    let new_pair = transform_pair pair in
    outcome_score new_pair + outcome_shape (snd new_pair)
  ;;

  let solve2 input = Ok (Int.to_string @@ total_score ~f:outcome2 input)
end
