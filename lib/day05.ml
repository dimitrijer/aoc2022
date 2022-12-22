open Base

module Solver : Solver.S = struct
  let day = 5

  (* Stack *)

  module Stack : sig
    type t

    val t_of_sexp : Sexp.t -> t
    val sexp_of_t : t -> Sexp.t
    val empty : t
    val push : t -> char -> t
    val push_n : t -> char list -> t
    val pop : t -> char * t
    val pop_n : t -> int -> char list * t
  end = struct
    type t = char list [@@deriving sexp]

    let empty = []
    let push s x = x :: s
    let push_n s xs = List.concat [ xs; s ]

    let pop = function
      | [] -> failwith "empty stack"
      | x :: s' -> x, s'
    ;;

    let pop_n = List.split_n

    let%test "push_n followed by pop_n" =
      let src = [ 'a'; 'b'; 'c' ]
      and dst = [ 'x'; 'y'; 'z' ] in
      let xs, src' = pop_n src 3 in
      let dst' = push_n dst xs in
      List.equal Char.equal src' []
      && List.equal Char.equal dst' [ 'a'; 'b'; 'c'; 'x'; 'y'; 'z' ]
    ;;
  end

  (* Input and parsing *)

  type move =
    { src : int
    ; dst : int
    ; num : int
    }
  [@@deriving sexp]

  type input =
    { stacks : Stack.t list
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
            |> List.fold ~init:Stack.empty ~f:Stack.push)
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
      let x, from_stack' = Stack.pop from_stack in
      let to_stack' = Stack.push to_stack x in
      Array.set stacks from_idx from_stack';
      Array.set stacks to_idx to_stack'
    done
  ;;

  let solve1 { stacks; moves } =
    let astacks = Array.of_list stacks in
    List.iter ~f:(apply_move astacks) moves;
    Ok
      (Array.fold astacks ~init:"" ~f:(fun s stack ->
         s ^ String.of_char (fst @@ Stack.pop stack)))
  ;;

  let apply_move2 stacks { src; dst; num } =
    let from_idx, to_idx = src - 1, dst - 1 in
    let from_stack, to_stack = Array.get stacks from_idx, Array.get stacks to_idx in
    let xs, from_stack' = Stack.pop_n from_stack num in
    let to_stack' = Stack.push_n to_stack xs in
    Array.set stacks from_idx from_stack';
    Array.set stacks to_idx to_stack'
  ;;

  let solve2 { stacks; moves } =
    let astacks = Array.of_list stacks in
    List.iter ~f:(apply_move2 astacks) moves;
    Ok
      (Array.fold astacks ~init:"" ~f:(fun s stack ->
         s ^ String.of_char (fst @@ Stack.pop stack)))
  ;;
end
