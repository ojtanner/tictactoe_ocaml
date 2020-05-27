type state
type field_coordinates

type player =
  | X
  | O
  | None

val create : unit -> state
val tuple_to_field_coordinates_exn : int * int -> field_coordinates
val get_current_player : state -> player
val execute_turn : state -> field_coordinates -> state
