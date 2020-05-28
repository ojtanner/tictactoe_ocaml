open Base
open Core
open Game

let row_to_string (row : int list) =
  let delimiter = "|" in
  let row =
    List.fold_left row ~init:delimiter ~f:(fun accumulator number ->
        let mark = if number = -1 then " x " else if number = 1 then " O " else "   " in
        let mark_with_delimiter = mark ^ delimiter in
        accumulator ^ mark_with_delimiter)
  in
  row ^ "\n"
;;

let rows_to_string (rows : int list list) =
  List.map rows ~f:(fun row -> row_to_string row)
;;

let generate_base_separator (row : int list) =
  let piece = '-' in
  let intersection = "+" in
  String.concat ~sep:intersection (List.map row ~f:(fun _ -> String.make 3 piece))
;;

let generate_separator (row : int list) delimiter =
  delimiter ^ generate_base_separator row ^ delimiter ^ "\n"
;;

let rec insert l str =
  match l with
  | [] -> []
  | [ hd ] -> hd :: insert [] str
  | hd :: tl -> hd :: str :: insert tl str
;;

let generate_rows_with_inner_separator playing_field =
  let rows_as_strings = rows_to_string playing_field in
  let first_row = List.nth playing_field 0 in
  match first_row with
  | Some row ->
    let innner_separator = generate_separator row "+" in
    insert rows_as_strings innner_separator
  | None -> []
;;

let playing_field_to_string_list playing_field =
  let first_row = List.nth playing_field 0 in
  match first_row with
  | Some row ->
    let outer_separator = generate_separator row "|" in
    let inner_string_list = generate_rows_with_inner_separator playing_field in
    [ outer_separator ] @ inner_string_list @ [ outer_separator ]
  | None -> [ "Empty Field passed \n" ]
;;

let print_playing_field ?(indent = 0) playing_field =
  let playing_field = playing_field_to_string_list playing_field in
  List.iter playing_field ~f:(fun row -> print_string (String.make indent '\t' ^ row))
;;

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
let render_game_state state = print_playing_field state.playing_field ~indent:2
