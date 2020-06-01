type t

val to_list : t -> string list
val create : unit -> t
val view : Fieldcoordinates.t -> t -> Player.t
val assoc : Fieldcoordinates.t -> Player.t -> t -> t
