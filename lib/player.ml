open Core

type t =
  | X
  | O
  | None

let to_string = function
  | X -> "X"
  | O -> "O"
  | None -> "-"
;;

let equal p1 p2 =
  let p1 = to_string p1
  and p2 = to_string p2 in
  equal_string p1 p2
;;
