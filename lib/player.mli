type t =
  | X
  | O
  | None

val to_string : t -> string
val equal : t -> t -> bool
