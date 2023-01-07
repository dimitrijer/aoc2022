open Base

module Solver : Solver.S = struct
  let day = 8

  type input =
    { rowm : int array array (* row-major *)
    ; colm : int array array (* column-major *)
    }
  [@@deriving sexp]

  let parse s =
    let lines = String.split_lines s in
    let rows =
      Array.of_list_map
        ~f:(fun line -> Array.of_list_map ~f:Char.get_digit_exn @@ String.to_list line)
        lines
    in
    Ok { rowm = rows; colm = Array.transpose_exn rows }
  ;;

  let map ~f a =
    Array.mapi
      ~f:(fun i row -> Array.mapi ~f:(fun j _ -> f (row, Array.get a.colm j) (i, j)) row)
      a.rowm
  ;;

  let is_visible (row, col) (x, y) =
    let dimx, dimy = Array.length row, Array.length col in
    let height = Array.get row y in
    let left_visible =
      Array.for_alli ~f:(fun j h -> j >= y || Int.equal y 0 || h < height) row
    and right_visible =
      Array.for_alli ~f:(fun j h -> j <= y || Int.equal y (dimy - 1) || h < height) row
    and up_visible =
      Array.for_alli ~f:(fun i h -> i >= x || Int.equal x 0 || h < height) col
    and down_visible =
      Array.for_alli ~f:(fun i h -> i <= x || Int.equal x (dimx - 1) || h < height) col
    in
    left_visible || right_visible || up_visible || down_visible
  ;;

  let solve1 a =
    let visibility = map ~f:is_visible a in
    Ok
      (Int.to_string
         (Array.fold
            ~init:0
            ~f:(fun acc row ->
              Array.fold ~init:acc ~f:(fun acc v -> acc + if v then 1 else 0) row)
            visibility))
  ;;

  let scenic_score (row, col) (x, y) =
    let dimx, dimy = Array.length row, Array.length col in
    let height = Array.get row y in
    let calc_score =
      Array.fold_until
        ~init:1
        ~f:(fun acc h -> if h >= height then Stop acc else Continue (acc + 1))
        ~finish:(fun acc -> acc - 1)
    in
    let left_score = Array.sub row ~pos:0 ~len:y |> Array.rev |> calc_score
    and right_score = Array.sub row ~pos:(y + 1) ~len:(dimy - y - 1) |> calc_score
    and up_score = Array.sub col ~pos:0 ~len:x |> Array.rev |> calc_score
    and down_score = Array.sub col ~pos:(x + 1) ~len:(dimx - x - 1) |> calc_score in
    left_score * right_score * up_score * down_score
  ;;

  let solve2 a =
    let scores = map ~f:scenic_score a in
    Ok
      (Int.to_string
      @@ Array.fold
           ~init:0
           ~f:(fun acc row -> Array.fold ~init:acc ~f:(fun acc v -> Int.max acc v) row)
           scores)
  ;;
end
