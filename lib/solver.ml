open Base

(** [S] modules are solvers for specific day. *)
module type S = sig
  (** [input] is type of input of the solver that is parsed from input string. *)
  type input

  val sexp_of_input : input -> Base.Sexp.t
  val input_of_sexp : Base.Sexp.t -> input

  (** Solver that solves puzzle for this specific [day]. *)
  val day : int

  (** [parse s] parses plaintext puzzle input [s]. *)
  val parse : string -> input Or_error.t

  (** [solve1] runs solver for part 1. *)
  val solve1 : input -> string Or_error.t

  (** Run [solve2] runs solver for part 2. *)
  val solve2 : input -> string Or_error.t
end
