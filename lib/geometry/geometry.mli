(** Geometry module contains basic mathematical utilities for the project *)

module Point = Point
module Direction = Direction
module Matrix = Matrix
module Transformations = Transformations 

type axis = Transformations.axis

(**
    Returns the value of the point in a given axis
*)
val value_at_axis : axis -> Point.point_t -> float




type transformation = 
  Rotation of Matrix.matrix_t * axis
  | Scale of float * float * float
  | Translation of float * float * float
  | ChangeBase of Matrix.matrix_t

val cartesian_of_spherical : float -> float -> Direction.direction_t -> Direction.direction_t
val cb_matrix_tangent : Direction.direction_t -> Point.point_t -> Matrix.matrix_t 