open Core

type t =
  { playing_field : Playingfield.t
  ; current_player : Player.t
  ; winner : Player.t
  }

let get_winner t = t.winner
let get_current_player t = t.current_player

let create () =
  { playing_field = Playingfield.create ()
  ; current_player = Player.X
  ; winner = Player.None
  }
;;

let info t =
  let open Player in
  let current_player = to_string t.current_player in
  let current_winner = to_string t.winner in
  [ "\n"
  ; "** Turn ~: **\n"
  ; "-------------\n"
  ; sprintf "| Player: %s |\n" current_player
  ; sprintf "| Winner: %s |\n" current_winner
  ; "~~~~~~~~~~~~~\n"
  ; "\n"
  ]
;;

let to_list t =
  let top_row = info t in
  let playing_field = Playingfield.to_list t.playing_field in
  let bottom_row = [ "\n"; "-------------\n" ] in
  top_row @ playing_field @ bottom_row
;;

let switch_player t =
  let open Player in
  match t.winner with
  | None ->
    (match t.current_player with
    | None -> t
    | X -> { t with current_player = O }
    | O -> { t with current_player = X })
  | X | O -> { t with current_player = None }
;;

let check_row_for_winner y t =
  let open Playingfield in
  let open Fieldcoordinates in
  let open Player in
  let first_coord = from_int 1 y in
  let second_coord = from_int 2 y in
  let third_coord = from_int 3 y in
  let playingfield = t.playing_field in
  match first_coord, second_coord, third_coord with
  | Ok a, Ok b, Ok c ->
    let first_el = view a playingfield
    and second_el = view b playingfield
    and third_el = view c playingfield in
    if equal first_el second_el && equal second_el third_el then first_el else None
  | _, _, _ -> None
;;

let check_column_for_winner x t =
  let open Playingfield in
  let open Fieldcoordinates in
  let open Player in
  let first_coord = from_int x 1 in
  let second_coord = from_int x 2 in
  let third_coord = from_int x 3 in
  let playingfield = t.playing_field in
  match first_coord, second_coord, third_coord with
  | Ok a, Ok b, Ok c ->
    let first_el = view a playingfield
    and second_el = view b playingfield
    and third_el = view c playingfield in
    if equal first_el second_el && equal second_el third_el then first_el else None
  | _, _, _ -> None
;;

let check_diagonals_for_winner t =
  let open Playingfield in
  let open Fieldcoordinates in
  let open Player in
  let left_upper_coord = from_int 1 1
  and left_lower_coord = from_int 1 3
  and right_upper_coord = from_int 3 1
  and right_lower_coord = from_int 3 3
  and middle_coord = from_int 2 2
  and playingfield = t.playing_field in
  match
    left_upper_coord, left_lower_coord, right_upper_coord, right_lower_coord, middle_coord
  with
  | Ok lu, Ok ll, Ok ru, Ok rl, Ok m ->
    let left_upper_el = view lu playingfield
    and left_lower_el = view ll playingfield
    and right_upper_el = view ru playingfield
    and right_lower_el = view rl playingfield
    and middle_el = view m playingfield in
    if equal left_upper_el middle_el && equal middle_el right_lower_el
    then left_upper_el
    else if equal right_upper_el middle_el && equal middle_el left_lower_el
    then right_upper_el
    else None
  | _, _, _, _, _ -> None
;;

let determine_winner t =
  let open Player in
  let row_1 = check_row_for_winner 1 t
  and row_2 = check_row_for_winner 2 t
  and row_3 = check_row_for_winner 3 t
  and col_1 = check_column_for_winner 1 t
  and col_2 = check_column_for_winner 2 t
  and col_3 = check_column_for_winner 3 t
  and diags = check_diagonals_for_winner t in
  let res = [ row_1; row_2; row_3; col_1; col_2; col_3; diags ] in
  let winner_list = List.filter res ~f:(fun el -> not (equal el None)) in
  match List.length winner_list with
  | 0 -> None
  | _ -> List.nth_exn winner_list 0
;;

let execute_turn t coord =
  let open Playingfield in
  let open Player in
  let winner = t.winner in
  if not (equal winner None)
  then t
  else (
    let current_player = t.current_player in
    let playing_field = t.playing_field in
    let updated_playing_field = assoc coord current_player playing_field in
    let t = { t with playing_field = updated_playing_field } in
    let winner = determine_winner t in
    let t = { t with winner } in
    switch_player t)
;;
