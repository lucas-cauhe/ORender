type hc
type axis = X | Y | Z

exception Change_basis_error of (Matrix.matrix_t * hc * string)

val hc_of_point : Point.point_t -> hc
val point_of_hc : hc -> Point.point_t
val hc_of_direction : Direction.direction_t -> hc
val direction_of_hc : hc -> Direction.direction_t

val translate : Matrix.matrix_t -> hc -> hc option
(** Returns [None] if homogenous coordinate was a [Direction] one *)

val translation_transformation_of_values :
  float -> float -> float -> Matrix.matrix_t

val scale : Matrix.matrix_t -> hc -> hc
val scale_transformation_of_values : float -> float -> float -> Matrix.matrix_t
val change_basis : Matrix.matrix_t -> hc -> hc

val cb_transformation_of_base :
  Direction.direction_t ->
  Direction.direction_t ->
  Direction.direction_t ->
  Point.point_t ->
  Matrix.matrix_t

val rotation_transformation_of_axis : angle:float -> axis -> Matrix.matrix_t
val rotate : Matrix.matrix_t -> axis -> hc -> hc
val combine_transformations : hc -> (hc -> hc) list -> hc
