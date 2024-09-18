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
    val string_of_direction : t -> string
    val eq : t -> t -> bool
end 


module Matrix : sig
    type t
    val identity : int -> t
    val transpose : t -> t
    val get_element : t -> int -> int -> float
    val multiply : t -> t -> t option
    val from_array_matrix : float array array -> t
    val string_of_matrix : t -> string
    
end 

(** Transformations module assumes matrix arguments are 4x4 matrices *)
module Transformations : sig
    type hc
    val hc_of_point : Point.t -> hc
    val hc_of_direction : Direction.t -> hc

    (** Returns [None] if homogenous coordinate was a [Direction] one *)
    val translate : Matrix.t -> hc -> hc option
    val scale : Matrix.t -> hc -> hc
    val rotation_transformation_of_axis : angle:float -> axis -> Matrix.t
    val rotate : Matrix.t -> axis -> hc -> hc
end