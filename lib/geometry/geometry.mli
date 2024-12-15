(** Geometry module contains basic mathematical utilities for the project *)

module Point = Point
module Direction = Direction
module Matrix = Matrix
module Transformations = Transformations

type axis = Transformations.axis

val value_at_axis : axis -> Point.point_t -> float
(**
    Returns the value of the point in a given axis
*)

type transformation =
  | Rotation of Matrix.matrix_t * axis
  | Scale of float * float * float
  | Translation of float * float * float
  | ChangeBase of Matrix.matrix_t
(** Defines the type of [transformation] to use, holding the required features to complete each transformation *)

val cartesian_of_spherical :
  float -> float -> Direction.direction_t -> Direction.direction_t
(** [cartesian_of_spherical latitude azimut normal] Converts spherical coordinates given by [latitude], [azimut] and [normal] to cartesian coordinates *)

val cb_matrix_tangent :
  Direction.direction_t -> Point.point_t -> Matrix.matrix_t
