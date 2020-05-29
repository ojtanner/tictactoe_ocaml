type t =
  { playing_field : Playingfield.t
  ; current_player : Player.t
  ; winner : Player.t
  }

(*

let create () =
  { playing_field = Playingfield.create ()
  ; current_player =
  ; winner =
  }
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

*)
