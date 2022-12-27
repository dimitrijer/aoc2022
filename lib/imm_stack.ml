open Base

module type STACK_ELEM = sig
  type t

  include Base.Sexpable.S with type t := t
  include Base.Comparable.S with type t := t
end

module type IMM_STACK = sig
  type t
  type element

  val t_of_sexp : Base.Sexp.t -> t
  val sexp_of_t : t -> Base.Sexp.t
  val empty : t
  val push : t -> element -> t
  val push_n : t -> element list -> t
  val pop : t -> element * t
  val pop_n : t -> int -> element list * t
  val peek : t -> element option
  val fold : init:'a -> f:('a -> element -> 'a) -> t -> 'a
  val compare : t -> t -> int
end

module Make (E : STACK_ELEM) : IMM_STACK with type element := E.t = struct
  type t = E.t list [@@deriving sexp]

  let empty = []
  let push s x = x :: s
  let push_n s xs = List.concat [ xs; s ]

  let pop = function
    | [] -> failwith "empty stack"
    | x :: s' -> x, s'
  ;;

  let peek = function
    | [] -> None
    | x :: _ -> Some x
  ;;

  let pop_n = List.split_n
  let fold ~init ~f s = List.fold ~init ~f s

  let%test "push_n followed by pop_n" =
    let src = [ 'a'; 'b'; 'c' ]
    and dst = [ 'x'; 'y'; 'z' ] in
    let xs, src' = pop_n src 3 in
    let dst' = push_n dst xs in
    List.equal Char.equal src' []
    && List.equal Char.equal dst' [ 'a'; 'b'; 'c'; 'x'; 'y'; 'z' ]
  ;;
end
