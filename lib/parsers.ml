open Base

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let integer = Angstrom.(take_while1 is_digit >>| Int.of_string <?> "integer")

let parse_string ~consume p s =
  match Angstrom.parse_string ~consume p s with
  | Ok x -> Base.Ok x
  | Error msg -> Base.Error (Base.Error.of_string msg)
;;
