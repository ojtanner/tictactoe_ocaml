open Core

let render_message ?(padding = 1) message =
  print_string (String.make padding '\t' ^ message);
  Out_channel.flush Out_channel.stdout
;;

let render_error ?(padding = 1) message =
  [ "\n"; ">> " ^ message ^ " <<"; "\n" ]
  |> List.iter ~f:(fun str -> render_message ~padding str)
;;

let generate_turn_info turn = [ "\n"; "** Turn  " ^ Int.to_string turn ^ " **\n" ]

let render_game ?(padding = 1) game_state turn =
  let open Game in
  let strings = to_list game_state in
  let strings = generate_turn_info turn @ strings in
  List.iter strings ~f:(fun el -> render_message ~padding el)
;;

let execute_turn game_state =
  let open Game in
  let open Player in
  let open Fieldcoordinates in
  let open Result in
  let player = get_current_player game_state in
  let message = to_string player ^ " mark : " in
  render_message message;
  match In_channel.input_line In_channel.stdin with
  | None ->
    let message = "No value entered. Try again\n" in
    Error message
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
         | Ok coord -> execute_turn game_state coord
         | Error msg ->
           let message = sprintf "%s" msg in
           Error message)
       | _ ->
         let message = sprintf "Please enter two spaced out integers\n" in
         Error message
     with
    | Failure _ ->
      let message = "Please enter two spaced out integers\n" in
      Error message)
;;

let start game_state =
  let open Player in
  let rec go game_state turn =
    render_game game_state turn;
    match Game.get_winner game_state with
    | (X | O) as winner ->
      let message = sprintf "%s won. Congratulations!\n\n" (to_string winner) in
      render_message message
    | None ->
      (match execute_turn game_state with
      | Ok game_state -> go game_state (turn + 1)
      | Error message ->
        render_error message;
        go game_state turn)
  in
  go game_state 1
;;

(*

Game -> Successul Turn : State
        Failure        : Error Message

I/O Wrapper -> Successful Turn : State
               Failure         : Error Message

I/O Wrapper -> Coordinate Input -> | Successful Turn ->  Game -> | Successful Turn -> State
                                                                 | Failure -> Error Message
                                   | Failure -> Error Message

*)
