open Tictactoe

let game_state = Game.create ()

let coordinates = Fieldcoordinates.from_int 1 1

let new_state =
  match coordinates with
  | Ok coordinates -> Game.execute_turn game_state coordinates
  | Error _ ->  game_state

let () =
  let field_string = Game.to_list new_state in
  List.iter (fun str -> print_string str) field_string
