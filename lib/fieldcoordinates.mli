type t
type n = | First | Second | Third

val from_int : int -> int -> (t, string) result
val get_col : t -> n
val get_row : t -> n
