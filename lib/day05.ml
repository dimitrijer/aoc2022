open Base

module Solver : Solver.S = struct
  let day = 5

  (* Input and parsing *)

  module S = Imm_stack.Make (Char)

  type move =
    { src : int
    ; dst : int
    ; num : int
    }
  [@@deriving sexp]

  type input =
    { stacks : S.t list
    ; moves : move list
    }
  [@@deriving sexp]

  let stacks_p =
    let open Angstrom in
    let elem_p = char '[' *> any_char <* char ']' in
    let empty_elem_p = char ' ' *> char ' ' <* char ' ' in
    let row_p = sep_by (char ' ') (elem_p <|> empty_elem_p) <* end_of_line in
    let construct_stacks rows =
      match List.transpose rows with
      | Some cols ->
        List.map
          ~f:(fun x ->
            x
            |> List.rev_filter ~f:(fun c -> not @@ Char.equal c ' ')
            |> List.fold ~init:S.empty ~f:S.push)
          cols
      | None -> failwith "mismatched number of rows"
    in
    construct_stacks
    <$> many row_p
    <* (sep_by (char ' ') (char ' ' *> Parsers.integer <* char ' ') <?> "ids")
    <* end_of_line
    <?> "stacks"
  ;;

  let move_p =
    let open Angstrom in
    let construct_move num src dst = { num; src; dst } in
    construct_move
    <$> string "move " *> Parsers.integer
    <* string " from "
    <*> Parsers.integer
    <*> string " to " *> Parsers.integer
    <?> "move"
  ;;

  let parse s =
    let open Angstrom in
    let construct_input stacks moves = { stacks; moves } in
    let p =
      construct_input <$> stacks_p <* end_of_line <*> many (move_p <* end_of_line)
    in
    Parsers.parse_string ~consume:All p s
  ;;

  (* Solution from here *)

  let apply_move stacks { src; dst; num } =
    let from_idx, to_idx = src - 1, dst - 1 in
    for _ = 1 to num do
      let from_stack, to_stack = Array.get stacks from_idx, Array.get stacks to_idx in
      let x, from_stack' = S.pop from_stack in
      let to_stack' = S.push to_stack x in
      Array.set stacks from_idx from_stack';
      Array.set stacks to_idx to_stack'
    done
  ;;

  let solve1 { stacks; moves } =
    let astacks = Array.of_list stacks in
    List.iter ~f:(apply_move astacks) moves;
    Ok
      (Array.fold astacks ~init:"" ~f:(fun s stack ->
         s ^ String.of_char (fst @@ S.pop stack)))
  ;;

  let apply_move2 stacks { src; dst; num } =
    let from_idx, to_idx = src - 1, dst - 1 in
    let from_stack, to_stack = Array.get stacks from_idx, Array.get stacks to_idx in
    let xs, from_stack' = S.pop_n from_stack num in
    let to_stack' = S.push_n to_stack xs in
    Array.set stacks from_idx from_stack';
    Array.set stacks to_idx to_stack'
  ;;

  let solve2 { stacks; moves } =
    let astacks = Array.of_list stacks in
    List.iter ~f:(apply_move2 astacks) moves;
    Ok
      (Array.fold astacks ~init:"" ~f:(fun s stack ->
         s ^ String.of_char (fst @@ S.pop stack)))
  ;;
end
