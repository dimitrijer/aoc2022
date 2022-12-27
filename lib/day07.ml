open Base

module Solver : Solver.S = struct
  let day = 7

  (* Immutable string stack to represent paths. *)
  module S = Imm_stack.Make (String)

  (** [fs] is filesystem type parametrized by type of directory metadata ['d]
      and file metadata ['f] *)
  type ('d, 'f) fs =
    | Dir of 'd * ('d, 'f) fs list
    | File of 'f
  [@@deriving sexp]

  (** [cmd] is type of command that is parsed from input.
     
      For parsing, we use [(string, string * int) fs] to represent a flat fs
      where directories have a name, and files have both name and size. *)
  type cmd =
    | Cd of string
    | Ls of (string, string * int) fs list (* (dirname, filename * file_size) *)
  [@@deriving sexp]

  type input = cmd list [@@deriving sexp]

  let fs_p =
    let open Angstrom in
    let construct_dir cs = Dir (String.of_char_list cs, []) in
    let construct_file i cs = File (String.of_char_list cs, i) in
    construct_dir
    <$> string "dir " *> many_till any_char end_of_line
    <|> (construct_file
        <$> (Parsers.integer <* char ' ')
        <*> many_till any_char end_of_line)
    <?> "fs"
  ;;

  let cmd_p =
    let open Angstrom in
    let construct_cd cs = Cd (String.of_char_list cs) in
    let construct_ls fss = Ls fss in
    construct_ls
    <$> string "$ ls" *> end_of_line *> many fs_p
    <|> (construct_cd <$> string "$ " *> string "cd " *> many_till any_char end_of_line)
    <?> "cmd"
  ;;

  let parse s = Angstrom.(Parsers.parse_string ~consume:Consume.All (many cmd_p) s)

  type state =
    { cwd : S.t (* stack of directories, the top one being current working dir *)
    ; fs : (S.t, string * int) fs (* (path as stack, file_name * file_size) *)
    }

  (** [cd s dir] changes cwd to dir] in state [s]. *)
  let cd s dir =
    { s with
      cwd =
        (match dir with
         | "/" -> S.push S.empty "/" (* can navigate to root from anywhere *)
         | ".." -> snd (S.pop s.cwd)
         | name -> S.push s.cwd name)
    }
  ;;

  (** [map f fs] applies [f] to each dir in [fs]. *)
  let rec map ~f = function
    | File _ as f -> f
    | Dir (meta, contents) ->
      let meta', contents' = f meta contents in
      Dir (meta', List.map ~f:(map ~f) contents')
  ;;

  (** [fold a f fs] folds [fs] with [f] and starting accumulator value of [a]. *)
  let fold ~init ~f fs =
    let rec recur acc = function
      | File _ -> acc
      | Dir (_, contents) as dir -> f (List.fold ~init:acc ~f:recur contents) dir
    in
    recur init fs
  ;;

  (** [ls s contents] sets contents of the directory at the top of cwd stack in
      state [s]. *)
  let ls s fss =
    (* convert parsed FS representation from (string, string * int) fs to
       (S.t, string * int) fs. *)
    let fss' =
      List.map ~f:(map ~f:(fun name contents -> S.push s.cwd name, contents)) fss
    in
    { s with
      fs =
        map
          ~f:(fun oldpath oldcontents ->
            if S.equal oldpath s.cwd then oldpath, fss' else oldpath, oldcontents)
          s.fs
    }
  ;;

  let get_size = function
    | File size -> size
    | Dir (size, _) -> size
  ;;

  (** [size fs] converts [(S.t, string * int) fs] to [(int, int) fs] by
      recursively calculating size of all directories. *)
  let rec size = function
    | File (_, size) -> File size
    | Dir (_, contents) ->
      let contents' = List.map ~f:size contents in
      let total_size = List.fold ~init:0 ~f:(fun a x -> a + get_size x) contents' in
      Dir (total_size, contents')
  ;;

  let run s cmd =
    match cmd with
    | Cd dir -> cd s dir
    | Ls fss -> ls s fss
  ;;

  let solve1 cmds =
    let fs =
      (List.fold ~init:{ cwd = S.empty; fs = Dir (S.push S.empty "/", []) } ~f:run cmds)
        .fs
    in
    let result =
      fold
        ~init:0
        ~f:(fun a fs ->
          match fs with
          | File _ -> a
          | Dir (dirsize, _) -> a + if dirsize < 100000 then dirsize else 0)
        (size fs)
    in
    Ok (Int.to_string @@ result)
  ;;

  let solve2 cmds =
    let fs =
      size
        (List.fold ~init:{ cwd = S.empty; fs = Dir (S.push S.empty "/", []) } ~f:run cmds)
          .fs
    in
    let used_space = get_size fs in
    let free_space = 70000000 - used_space
    and needed_space = 30000000 in
    let min_size_to_delete = needed_space - free_space in
    let sizes_big_enough =
      fold
        ~init:[]
        ~f:(fun l fs ->
          match fs with
          | File _ -> l
          | Dir (size, _) -> if size > min_size_to_delete then size :: l else l)
        fs
    in
    Ok
      (Int.to_string
      @@ List.fold ~init:(List.hd_exn sizes_big_enough) ~f:Int.min sizes_big_enough)
  ;;
end
