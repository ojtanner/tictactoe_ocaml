exception Invalid_field_coordinates of string
exception Occupied_field_coordinates of string

type playing_field = int list list

type player =
  | X
  | O
  | None

type field_coordinates = int * int

type state =
  { playing_field : playing_field
  ; current_player : player
  ; winner : player
  }

let create () =
  { playing_field = List.init 3 (fun _ -> List.init 3 (fun _ -> 0))
  ; current_player = X
  ; winner = None
  }
;;

let invalid_coordinates_error_message = "X and Y must be between 1 and 3"
let get_current_player state = state.current_player

let tuple_to_field_coordinates_exn (x, y) =
  if (x < 1 || x > 3) || y < 1 || y > 3
  then raise (Invalid_field_coordinates invalid_coordinates_error_message)
  else x, y
;;

let switch_player state =
  match state.winner with
  | None ->
    (match state.current_player with
    | None -> state
    | X -> { state with current_player = O }
    | O -> { state with current_player = X })
  | X | O -> { state with current_player = None }
;;

let get_Field_from_coordinates state field_coordinates =
  let x, y = field_coordinates in
  let field = List.nth (List.nth state.playing_field x) y in
  field
;;

let execute_turn state field_coordinates =
  if state.winner != None
  then state
  else (
    let field = get_Field_from_coordinates state field_coordinates in
    match field with
    | 0 -> switch_player state
    | _ -> raise (Occupied_field_coordinates "Field occupied"))
;;
