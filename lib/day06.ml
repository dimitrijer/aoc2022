open Base

module Solver : Solver.S = struct
  let day = 6

  type input = string [@@deriving sexp]

  let parse s = Ok s

  type state =
    { num_seen : int
    ; n : int (* number of different characters in preamble *)
    ; last_n : char list (* buffer of last n characters *)
    }

  let is_backfill_done { last_n; n; _ } = List.length last_n >= n

  let is_preamble_done s =
    if is_backfill_done s
    then (
      let num_unique = Set.length (Set.of_list (module Char) s.last_n) in
      Int.equal num_unique s.n)
    else false
  ;;

  let shift c n cs = if List.length cs < n then c :: cs else c :: List.drop_last_exn cs

  let accept c =
    let open State.Let_syntax in
    let%bind s = State.get () in
    let s' = { s with last_n = shift c s.n s.last_n; num_seen = s.num_seen + 1 } in
    let%bind _ = State.put s' in
    return (is_preamble_done s')
  ;;

  let rec process cs =
    let open State.Let_syntax in
    match cs with
    | [] -> return None
    | hd :: tl ->
      if%bind accept hd
      then (
        let%bind { num_seen; _ } = State.get () in
        return @@ Some num_seen)
      else process tl
  ;;

  let solve1 s =
    match process (String.to_list s) { last_n = []; num_seen = 0; n = 4 } with
    | Some num_seen, _ -> Ok (Int.to_string num_seen)
    | None, _ -> Error (Error.of_string "no preamble")
  ;;

  let solve2 s =
    match process (String.to_list s) { last_n = []; num_seen = 0; n = 14 } with
    | Some num_seen, _ -> Ok (Int.to_string num_seen)
    | None, _ -> Error (Error.of_string "no preamble")
  ;;
end
