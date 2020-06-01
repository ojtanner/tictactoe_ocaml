type t

(** [create ()] creates a new game-state. *)
val create : unit -> t

val to_list : t -> string list

val execute_turn : t -> Fieldcoordinates.t -> t
