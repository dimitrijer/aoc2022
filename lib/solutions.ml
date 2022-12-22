module M = Map.Make (Int)

let solvers =
  M.empty
  |> M.add Day01.Solver.day (module Day01.Solver : Solver.S)
  |> M.add Day02.Solver.day (module Day02.Solver : Solver.S)
  |> M.add Day03.Solver.day (module Day03.Solver : Solver.S)
  |> M.add Day04.Solver.day (module Day04.Solver : Solver.S)
  |> M.add Day05.Solver.day (module Day05.Solver : Solver.S)
;;

let solve day input =
  match M.find_opt day solvers with
  | Some (module M) ->
    let open Base.Or_error.Let_syntax in
    let%bind input = M.parse input in
    let%bind sol1 = M.solve1 input in
    let%bind sol2 = M.solve2 input in
    return @@ sol1 ^ "\n" ^ sol2
  | None -> Base.Or_error.errorf "no solver for day %d" day
;;
