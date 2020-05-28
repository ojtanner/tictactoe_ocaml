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

let generate_base_separator (row : int list) =
  let piece = '-' in
  let intersection = "+" in
  String.concat ~sep:intersection (List.map row ~f:(fun _ -> String.make 3 piece))
;;

let generate_row_separator (row : int list) =
  let delimiter = "|" in
  delimiter ^ generate_base_separator row ^ delimiter ^ "\n"
;;

let generate_top_bottom_separator (row : int list) =
  let delimiter = "+" in
  delimiter ^ generate_base_separator row ^ delimiter ^ "\n"
;;

let print_playing_field playing_field =
  let separator = generate_row_separator [ 0; 0; 0 ] in
  let top_bottom_separator = generate_top_bottom_separator [ 0; 0; 0 ] in
  let rows = List.map playing_field ~f:(fun row -> row_to_string row) in
  let rows = String.concat ~sep:separator rows in
  print_string (top_bottom_separator ^ rows ^ top_bottom_separator)
;;

let render_game_state state =
  print_playing_field state.playing_field
