(** Geometry's Matrix module *)

type matrix_t
(** Internal matrix representation *)

val identity : int -> matrix_t
(** [identity n] returns a nxn identity matrix *)

val transpose : matrix_t -> matrix_t
(** Transpose the given [matrix] *)

val get_element : matrix_t -> int -> int -> float
(** [get_element mat i j] returns the element at (i,j) square of the matrix in a 2d representation *)

val multiply : matrix_t -> matrix_t -> matrix_t option
(** Matrix multiplication 

    Returns [None] if matrices have incorrect dimensions
*)

val from_array_matrix : float array array -> matrix_t
(** Builds a matrix from the given 2d representation *)

val string_of_matrix : matrix_t -> string
(** Prettify the given matrix *)

val inverse : matrix_t -> matrix_t option
(** Compute the inverse of a matrix if that is doable *)
