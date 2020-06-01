open Tictactoe

let game_state = Game.create ()

let coordinates_1 = Fieldcoordinates.from_int 1 1

let turn_1 =
  match coordinates_1 with
  | Ok coordinates -> Game.execute_turn game_state coordinates
  | Error message ->  print_endline message; game_state

let () =
  let field_string = Game.to_list turn_1 in
  List.iter (fun str -> print_string str) field_string

let coordinates_2 = Fieldcoordinates.from_int 1 2

let turn_2 =
  match coordinates_2 with
  | Ok coordinates -> Game.execute_turn turn_1 coordinates
  | Error message ->  print_endline message; game_state

let () =
  let field_string = Game.to_list turn_2 in
  List.iter (fun str -> print_string str) field_string

let coordinates_3 = Fieldcoordinates.from_int 3 2

let turn_3 =
  match coordinates_3 with
  | Ok coordinates -> Game.execute_turn turn_2 coordinates
  | Error message ->  print_endline message; game_state

let () =
  let field_string = Game.to_list turn_3 in
  List.iter (fun str -> print_string str) field_string
