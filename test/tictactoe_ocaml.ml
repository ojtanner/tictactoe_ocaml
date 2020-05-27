open OUnit2
open Tictactoe

let game_tests = "test suite for Game" >::: [
    "switch player after legal move" >:: (fun _ ->
        let initial_state = Game.create () in
        let next_state = Game.execute_turn
            initial_state
            (Game.tuple_to_field_coordinates_exn (1,1)) in
        let current_player = Game.get_current_player next_state and
          expected_player = Game.O in
        assert_equal expected_player current_player
      )
  ]

let _ = run_test_tt_main game_tests
