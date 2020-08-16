open Core

type n =
  | First
  | Second
  | Third

type t = n * n

(* This is a hack. Not sure how to exactly specify 1 - 3. Maybe exn *)
let int_to_n num =
  match num with
  | 1 -> First
  | 2 -> Second
  | 3 | _ -> Third
;;

let from_int x y =
  let x_error =
    if x < 1 || x > 3
    then sprintf "X outside of field, must be between 1 and 3 but is %i\n" x
    else ""
  in
  let y_error =
    if y < 1 || y > 3
    then sprintf "Y outside of field, must be between 1 and 3 but is %i\n" x
    else ""
  in
  let coord_error = x_error ^ y_error in
  match String.length coord_error with
  | 0 -> Ok (int_to_n x, int_to_n y)
  | _ -> Error coord_error
;;

let get_col t =
  let first, _ = t in
  first
;;

let get_row t =
  let _, second = t in
  second
;;
