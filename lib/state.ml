(* State monad *)

let bind ma ~f state =
  let a, s' = ma state in
  f a s'
;;

let map ma ~f state =
  let a, s' = ma state in
  f a, s'
;;

let return a state = a, state

let apply mf ma state =
  let f, s' = mf state in
  map ~f ma s'
;;

module Monad_arg = struct
  type ('a, 's) t = 's -> 'a * 's

  let return = return
  let apply = apply
  let map = `Custom map
  let bind = bind
end

include Core.Monad.Make2 (Monad_arg)
include Core.Applicative.Make2 (Monad_arg)

let get () s = s, s
let put s _ = (), s
