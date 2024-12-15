type hc
(** Internal representation for an homogeneous coordinate *)

type axis = X | Y | Z
(** Axis representation *)

exception Change_basis_error of (Matrix.matrix_t * hc * string)

val hc_of_point : Point.point_t -> hc
(** Turns a point into an homogenous coordinate *)

val point_of_hc : hc -> Point.point_t
(** Returns a [point] from the given homogeneous coordinate *)

val hc_of_direction : Direction.direction_t -> hc
(** Turns a direction into an homogenous coordinate *)

val direction_of_hc : hc -> Direction.direction_t
(** Returns a [direction] from the given homogeneous coordinate *)

val translate : Matrix.matrix_t -> hc -> hc option
(** Returns [None] if homogenous coordinate was a [Direction] one *)

val translation_transformation_of_values :
  float -> float -> float -> Matrix.matrix_t
(** Builds a translation matrix from the given translation values *)

val scale : Matrix.matrix_t -> hc -> hc
(** [scale scale_mat hc] scales the [hc] by [scale_mat] *)

val scale_transformation_of_values : float -> float -> float -> Matrix.matrix_t
(** Builds a scaling matrix from the given scaling values *)

val change_basis : Matrix.matrix_t -> hc -> hc
(** [change_basis cb_mat hc] performs change of basis transformation on the given [hc] *)

val cb_transformation_of_base :
  Direction.direction_t ->
  Direction.direction_t ->
  Direction.direction_t ->
  Point.point_t ->
  Matrix.matrix_t
(** Builds a change of base matrix from the given change of base values *)

val rotation_transformation_of_axis : angle:float -> axis -> Matrix.matrix_t
val rotate : Matrix.matrix_t -> axis -> hc -> hc
val combine_transformations : hc -> (hc -> hc) list -> hc
