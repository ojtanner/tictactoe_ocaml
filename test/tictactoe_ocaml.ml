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
         let actual_player =
           match from_int 1 1 >>= fun coord -> execute_turn game coord with
           | Ok game -> get_current_player game
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
         let actual_winner =
           from_int 1 1
           >>= fun t1 ->
           from_int 2 1
           >>= fun t2 ->
           from_int 1 2
           >>= fun t3 ->
           from_int 2 2
           >>= fun t4 ->
           from_int 1 3
           >>= fun t5 ->
           print_game game;
           execute_turn game t1
           >>= fun turn1 ->
           print_game turn1;
           execute_turn turn1 t2
           >>= fun turn2 ->
           print_game turn2;
           execute_turn turn2 t3
           >>= fun turn3 ->
           print_game turn3;
           execute_turn turn3 t4
           >>= fun turn4 ->
           print_game turn4;
           execute_turn turn4 t5
           >>| fun turn5 ->
           print_game turn5;
           get_winner turn5
         in
         let actual_winner =
           match actual_winner with
           | Ok winner -> winner
           | _ -> None
         in
         assert_equal expected_winner actual_winner)
       ]
;;

let _ = run_test_tt_main game_tests
