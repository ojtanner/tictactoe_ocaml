open Core

type t = int * int

let from_int x y =
  let x_error =
    if x < 1 || x > 3
    then sprintf "X outside of field, must be between 1 and 3 but is %i" x
    else ""
  in
  let y_error =
    if y < 1 || y > 3
    then sprintf "Y outside of field, must be between 1 and 3 but is %i" x
    else ""
  in
  let coord_error = x_error ^ y_error in
  match String.length coord_error with
  | 0 -> Ok (x, y)
  | _ -> Error coord_error
;;
