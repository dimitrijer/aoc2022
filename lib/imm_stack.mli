(** [STACK_ELEM] can be pushed onto immutable stack. *)
module type STACK_ELEM = sig
  (** [t] is type of element that's stored on stack. *)
  type t

  include Base.Sexpable.S with type t := t
  include Base.Comparable.S with type t := t
end

(** [IMM_STACK] is a stack that's immutable. *)
module type IMM_STACK = sig
  (** [t] is type of stack. *)
  type t

  (** [element] is type of elements stored on stack. *)
  type element

  (** [t_of_sexp] creates a stack from S-expression. *)
  val t_of_sexp : Base.Sexp.t -> t

  (** [sexp_of_t] serializes a stack to S-expression. *)
  val sexp_of_t : t -> Base.Sexp.t

  (** [empty] is an empty stack. *)
  val empty : t

  (** [push s x] pushes [x] on top of [s]. *)
  val push : t -> element -> t

  (** [push_n s xs] pushes all [xs] on top of [s], retaining order. *)
  val push_n : t -> element list -> t

  (** [pop s] pops top element from [s], and returns it and updates stack. *)
  val pop : t -> element * t

  (** [pop_n s n] pops top [n] elements from [s], and returns those elements
      and updated stack. *)
  val pop_n : t -> int -> element list * t

  (** [peek s] peeks top element on [s], returns [None] if stack is empty. *)
  val peek : t -> element option

  (** [fold s ~init ~f] folds elements of stack to single value using [f] and 
      [init] as initial accumulator value. *)
  val fold : init:'a -> f:('a -> element -> 'a) -> t -> 'a

  (** [equal l r] returns true if [l] and [r] contain same elements, in same
      order. *)
  val equal : t -> t -> bool
end

(** [Make] is a functor that creates a stack module for specified element
    type. *)
module Make (E : STACK_ELEM) : IMM_STACK with type element := E.t
