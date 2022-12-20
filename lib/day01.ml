open Base

module Solver : Solver.S = struct
  let day = 1

  type input = int list list [@@deriving sexp]

  let parse s =
    let lines = String.split_lines s in
    let ints =
      List.map lines ~f:(fun x ->
        try Int.of_string x with
        | _ -> 0)
    in
    Ok (List.group ints ~break:(fun _ r -> Int.equal r 0))
  ;;

  let to_sorted_sums input =
    let sums = List.map ~f:(fun xs -> List.fold ~init:0 ~f:( + ) xs) input in
    List.sort ~compare:(fun l r -> -Int.compare l r) sums
  ;;

  let solve1 input =
    match to_sorted_sums input with
    | [] -> Error (Error.of_string "no elements")
    | x :: _ -> Ok (Int.to_string x)
  ;;

  let solve2 input =
    let sum3 = List.fold ~init:0 ~f:( + ) (List.take (to_sorted_sums input) 3) in
    Ok (Int.to_string sum3)
  ;;
end
