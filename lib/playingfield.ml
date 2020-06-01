open Base

type row = Player.t * Player.t * Player.t
type t = row * row * row

let create () =
  let open Player in
  let row = None, None, None in
  row, row, row
;;

let take n (first, second, third) =
  let open Fieldcoordinates in
  match n with
  | First -> first
  | Second -> second
  | Third -> third

let update n data (first, second, third) =
  let open Fieldcoordinates in
  match n with
  | First -> (data, second, third)
  | Second -> (first, data, third)
  | Third -> (first, second, data)

let view coord t =
  let open Fieldcoordinates in
  let row_coord = get_row coord in
  let col_coord = get_col coord in
  let t_row = take row_coord t in
  let t_col = take col_coord t_row in
  t_col

let assoc coord data t =
  let open Fieldcoordinates in
  let row_coord = get_row coord in
  let col_coord = get_col coord in
  let t_row = take row_coord t in
  let new_row = update col_coord data t_row in
  update row_coord new_row t

let row_to_string ((first, second, third) : row) =
  let delimiter = "|" in
  let to_string player = " " ^ Player.to_string player ^ " " in
  let row =
    delimiter
    ^ to_string first
    ^ delimiter
    ^ to_string second
    ^ delimiter
    ^ to_string third
    ^ delimiter
  in
  row ^ "\n"
;;

let rows_to_string_list ((first, second, third) : t) =
  [ row_to_string first ] @ [ row_to_string second ] @ [ row_to_string third ]
;;

let generate_base_separator () = "---+---+---"

let generate_separator delimiter =
  delimiter ^ generate_base_separator () ^ delimiter ^ "\n"
;;

let rec insert l str =
  match l with
  | [] -> []
  | [ hd ] -> hd :: insert [] str
  | hd :: tl -> hd :: str :: insert tl str
;;

let generate_rows_with_inner_separator t =
  let rows_as_strings = rows_to_string_list t in
  let innner_separator = generate_separator "|" in
  insert rows_as_strings innner_separator
;;

let to_list t =
  let outer_separator = generate_separator "+" in
  let inner_string_list = generate_rows_with_inner_separator t in
  [ outer_separator ] @ inner_string_list @ [ outer_separator ]
;;
