open Core
open Game
open Fieldcoordinates
open Player

let render_message ?(padding = 1) message =
  print_string (String.make padding '\t' ^ message);
  Out_channel.flush Out_channel.stdout
;;

let render_game ?(padding = 1) game_state =
  let strings = to_list game_state in
  List.iter strings ~f:(fun el -> render_message ~padding el)
;;

let rec loop game_state =
  render_game game_state;
  let winner = get_winner game_state in
  match winner with
  | (X | O) as winner ->
    let message = sprintf "%s won. Congratulations!\n\n" (to_string winner) in
    render_message message
  | None ->
    let message = "Enter mark : " in
    render_message message;
    (match In_channel.input_line In_channel.stdin with
    | None ->
      let message = "No value entered. Try again\n" in
      render_message message;
      loop game_state
    | Some line ->
      (try
         line
         |> String.strip
         |> String.split ~on:' '
         |> List.map ~f:(fun el -> Int.of_string el)
         |> fun coords ->
         match coords with
         | x :: y :: _ ->
           let turn_coords = from_int x y in
           (match turn_coords with
           | Ok coord -> loop (execute_turn game_state coord)
           | Error msg ->
             printf "%s" msg;
             loop game_state)
         | _ ->
           printf "Please enter two spaced out integers\n";
           loop game_state
       with
      | Failure _ ->
        let message = "Please enter two spaced out integers\n" in
        render_message message;
        loop game_state))
;;
