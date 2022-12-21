open Base

module Solver : Solver.S = struct
  let day = 4

  type input = ((int * int) * (int * int)) list [@@deriving sexp]

  let construct_pair l r = l, r

  let parse s =
    let open Angstrom in
    let range_p = construct_pair <$> Parsers.integer <* char '-' <*> Parsers.integer in
    let ranges_p = construct_pair <$> range_p <* char ',' <*> range_p in
    let p = many (ranges_p <* end_of_line) in
    Parsers.parse_string ~consume:Consume.All p s
  ;;

  let is_fully_contained ((l1, r1), (l2, r2)) =
    (l1 <= l2 && r1 >= r2) || (l2 <= l1 && r2 >= r1)
  ;;

  let solve1 ranges =
    let count = List.count ~f:is_fully_contained ranges in
    Ok (Int.to_string count)
  ;;

  let is_overlap ((l1, r1), (l2, r2)) =
    let is_left_overlap = (l1 >= l2 && l1 <= r2) || (r1 >= l2 && r1 <= r2) in
    let is_right_overlap = (l2 >= l1 && l2 <= r1) || (r2 >= l1 && r2 <= r1) in
    is_left_overlap || is_right_overlap
  ;;

  let solve2 ranges =
    let count = List.count ~f:is_overlap ranges in
    Ok (Int.to_string count)
  ;;
end
