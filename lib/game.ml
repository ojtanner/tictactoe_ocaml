open Core

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

let info t =
  let open Player in
  let current_player = to_string t.current_player in
  let current_winner = to_string t.winner in
  [ "\n\n*** Game-Info: ***\n------------------\n"
    ^ sprintf "Next player: %s\n" current_player
    ^ sprintf "Winner: %s\n" current_winner
    ^ "\n"
  ]
;;

let to_list t =
  let top_row = info t in
  let playing_field = Playingfield.to_list t.playing_field in
  let bottom_row = [ "\n\n*** Placeholder ***\n\n" ] in
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

let execute_turn t coord =
  let open Playingfield in
  let current_player = t.current_player in
  let playing_field = t.playing_field in
  let updated_playing_field = assoc coord current_player playing_field in
  let t = { t with playing_field = updated_playing_field } in
  switch_player t
;;
