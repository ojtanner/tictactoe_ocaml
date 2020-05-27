open Tictactoe

let initial_state = Game.create ()

let next_state = Game.execute_turn
    initial_state
    (Game.tuple_to_field_coordinates_exn (1,1))

let () =
  Cli.print_playing_field next_state.playing_field
