open Base

module Solver : Solver.S = struct
  let day = 3

  type input = string list [@@deriving sexp]

  let parse s = Ok (String.split_lines s)
  let uniq s = Set.of_list (module Char) (String.to_list s)

  let priority = function
    | None -> 0
    | Some c ->
      if Char.is_uppercase c
      then 27 + Char.to_int c - Char.to_int 'A'
      else 1 + Char.to_int c - Char.to_int 'a'
  ;;

  let find_common s =
    let count = String.length s in
    let p1, p2 = String.prefix s (count / 2), String.suffix s (count / 2) in
    let s1, s2 = uniq p1, uniq p2 in
    match Set.inter s1 s2 |> Set.to_list with
    | [] -> None
    | x :: _ -> Some x
  ;;

  let solve1 rucksacks =
    let total_prio =
      List.fold ~init:0 ~f:(fun a s -> a + priority (find_common s)) rucksacks
    in
    Ok (Int.to_string total_prio)
  ;;

  let uniql l =
    let unique_items = List.map ~f:uniq l in
    List.fold
      ~init:(Set.empty (module Char))
      ~f:(fun a x -> if Set.is_empty a then x else Set.inter a x)
      unique_items
  ;;

  let solve2 rucksacks =
    let sets = List.map ~f:uniql (List.chunks_of ~length:3 rucksacks) in
    let sum =
      List.fold
        ~init:0
        ~f:(fun acc s -> acc + priority (s |> Set.to_list |> List.hd))
        sets
    in
    Ok (Int.to_string sum)
  ;;
end
