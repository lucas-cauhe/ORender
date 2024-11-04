(** Geometry module contains basic mathematical utilities for the project *)

type axis = X | Y | Z

(** Associate functions to work with 3D points *)
module Point : sig
    (** Point internal type *)
    type t

    val x : t -> float
    val y : t -> float
    val z : t -> float

    (** Sum of two points *)
    val sum : t -> t -> t

    (** Subtraction of two points *)
    val sub : t -> t -> t

    (** Product between point and scalar value *)
    val prod : t -> float -> t

    (** Division between point and scalar value.
        Returns [None] if [num] is 0 *)
    val div : t -> float -> t option
    val from_coords : float -> float -> float -> t
    val string_of_point : t -> string
    val eq : t -> t -> bool
    val ( + ) : t -> t -> t
    val ( - ) : t -> t -> t
    val distance : t -> t -> float

    (**
        Given a list of points return the mean on each axis of all of them
        Returns [None] if the list is empty
    *)
    val mean : t list -> t option 

    (**
        Returns the value of the point in a given axis
    *)
    val value_at_axis : axis -> t -> float
end 

module Direction : sig
    type t
    val x : t -> float
    val y : t -> float
    val z : t -> float
    val sum : t -> t -> t
    val sub : t -> t -> t
    val prod : t -> float -> t
    val div : t -> float -> t option
    val dot : t -> t -> float
    val modulus : t -> float
    val normalize : t -> t option
    val cross_product : t -> t -> t
    val from_coords : float -> float -> float -> t
    val of_point : Point.t -> t
    val between_points : Point.t -> Point.t -> t
    val string_of_direction : t -> string
    val eq : t -> t -> bool

    val ( /* ) : t -> t -> t
    val ( * ) : t -> t -> float
end 


module Matrix : sig
    type t
    val identity : int -> t
    val transpose : t -> t
    val get_element : t -> int -> int -> float
    val multiply : t -> t -> t option
    val from_array_matrix : float array array -> t
    val string_of_matrix : t -> string
    val inverse : t -> t option
    
end 

type transformation = 
  Rotation of Matrix.t * axis
  | Scale of float * float * float
  | Translation of float * float * float
  | ChangeBase of Matrix.t

(** Transformations module assumes matrix arguments are 4x4 matrices *)
module Transformations : sig
    type hc
    exception Change_basis_error of (Matrix.t * hc * string)
    val hc_of_point : Point.t -> hc
    val point_of_hc : hc -> Point.t
    val hc_of_direction : Direction.t -> hc
    val direction_of_hc : hc -> Direction.t

    (** Returns [None] if homogenous coordinate was a [Direction] one *)
    val translate : Matrix.t -> hc -> hc option
    val translation_transformation_of_values : float -> float -> float -> Matrix.t
    val scale : Matrix.t -> hc -> hc
    val scale_transformation_of_values : float -> float -> float -> Matrix.t
    val rotation_transformation_of_axis : angle:float -> axis -> Matrix.t
    val rotate : Matrix.t -> axis -> hc -> hc
    val change_basis : Matrix.t -> hc -> hc
    val cb_transformation_of_base : Direction.t -> Direction.t -> Direction.t -> Point.t -> Matrix.t
    val combine_transformations : hc -> (hc -> hc) list -> hc
end

val cartesian_of_spherical : float -> float -> Direction.t -> Point.t -> Direction.t