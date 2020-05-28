open Tictactoe

let initial_state = Game.create ()

let next_state = Game.execute_turn
    initial_state
    (Game.tuple_to_field_coordinates_exn (1,1))

let () =
  Cli.render_game_state next_state
