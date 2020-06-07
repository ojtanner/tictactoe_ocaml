open Core
open Game
open Fieldcoordinates
open Player

let render_game ?(padding = 1) game_state =
  let strings = to_list game_state in
  List.iter strings ~f:(fun el -> print_string ((String.make padding '\t') ^ el))

let rec loop game_state =
  render_game game_state;
  let winner = get_winner game_state in
  match winner with
  | X | O  as winner -> printf "%s won. Congratulations!\n" (to_string winner)
  | None ->
  print_string "Mark : ";
  Out_channel.flush Out_channel.stdout;
  match In_channel.input_line In_channel.stdin with
  | None ->
    print_string "No value entered. Try again\n";
    Out_channel.flush Out_channel.stdout;
    loop game_state
  | Some line ->
    let args = String.split line ~on:' ' in
    let args_as_ints = List.map args ~f:(fun el -> Int.of_string el) in
    match args_as_ints with
    | x :: y :: _  -> (
        let turn_coords = from_int x y in
        match turn_coords with
        | Ok coord -> loop (execute_turn game_state coord)
        | Error msg -> printf "%s" msg; loop game_state
      )
    | _ -> printf "Please enter two spaced out integers\n"; loop game_state
;;
