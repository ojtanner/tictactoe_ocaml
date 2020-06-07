open Core
open OUnit2
open Tictactoe

let print_game t =
  let open Game in
  List.iter (to_list t) ~f:(fun str -> print_string str)
;;

let game_tests =
  "test-suite for module Game"
  >::: [ ("Player changes after turn"
         >:: fun _ ->
         let open Result in
         let open Fieldcoordinates in
         let open Game in
         let open Player in
         let expected_player = O in
         let game = create () in
         let t1 = from_int 1 1 in
         let actual_player =
           match t1 with
           | Ok t1 -> execute_turn game t1 |> get_current_player
           | _ -> None
         in
         assert_equal expected_player actual_player)
       ; ("X wins with row 1"
         >:: fun _ ->
         let open Result in
         let open Fieldcoordinates in
         let open Game in
         let open Player in
         let game = create () in
         let expected_winner = X in
         let t1 = from_int 1 1
         and t2 = from_int 2 1
         and t3 = from_int 1 2
         and t4 = from_int 2 2
         and t5 = from_int 1 3
         and t6 = from_int 2 2 in
         let actual_winner =
           match t1, t2, t3, t4, t5, t6 with
           | Ok t1, Ok t2, Ok t3, Ok t4, Ok t5, Ok t6 ->
             print_game game;
             let turn1 = execute_turn game t1 in
             print_game turn1;
             let turn2 = execute_turn turn1 t2 in
             print_game turn2;
             let turn3 = execute_turn turn2 t3 in
             print_game turn3;
             let turn4 = execute_turn turn3 t4 in
             print_game turn4;
             let turn5 = execute_turn turn4 t5 in
             print_game turn5;
             let turn6 = execute_turn turn5 t6 in
             print_game turn6;
             get_winner turn6
           | _, _, _, _, _, _ -> None
         in
         assert_equal expected_winner actual_winner)
       ]
;;

let _ = run_test_tt_main game_tests
