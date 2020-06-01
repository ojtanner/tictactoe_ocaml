type t =
  | X
  | O
  | None

let to_string = function
  | X -> "X"
  | O -> "O"
  | None -> " "
