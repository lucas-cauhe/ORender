(** Geometry's Matrix module *)

type matrix_t
val identity : int -> matrix_t
val transpose : matrix_t -> matrix_t
val get_element : matrix_t -> int -> int -> float
val multiply : matrix_t -> matrix_t -> matrix_t option
val from_array_matrix : float array array -> matrix_t
val string_of_matrix : matrix_t -> string
val inverse : matrix_t -> matrix_t option