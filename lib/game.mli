type t

(** [create ()] creates a new game-state. *)
val create : unit -> t

val get_winner : t -> Player.t
val get_current_player : t -> Player.t

(** [to_list t] takes the state t of a game and converts that state to a list of strings that can be printed. *)
val to_list : t -> string list

(** [execute_turn t coord] takes the state t of a game and the fieldcoordinates coord and then executes current turn based off of the current player and the coord of the playingfield he specified. *)
val execute_turn : t -> Fieldcoordinates.t -> (t, string) result
