type t =
  { playing_field : Playingfield.t
  ; current_player : Player.t
  ; winner : Player.t
  }

let create () =
  { playing_field = Playingfield.create ()
  ; current_player = Player.X
  ; winner = Player.None
  }
;;

let to_list t =
  let top_row = ["\n\n*** Placeholder ***\n\n"] in
  let playing_field = Playingfield.to_list t.playing_field in
  let bottom_row = ["\n\n*** Placeholder ***\n\n"] in
  top_row @ playing_field @ bottom_row


let switch_player state =
  let open Player in
  match state.winner with
  | None ->
    (match state.current_player with
    | None -> state
    | X -> { state with current_player = O }
    | O -> { state with current_player = X })
  | X | O -> { state with current_player = None }
;;

let execute_turn t coord =
  let open Playingfield in
  let current_player = t.current_player in
  let playing_field = t.playing_field in
  let updated_playing_field = assoc coord current_player playing_field in
  let t = { t with playing_field = updated_playing_field } in
  switch_player t
