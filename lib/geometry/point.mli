(** Associate functions to work with 3D points *)

(** Point internal type *)
type point_t 

val x : point_t -> float
val y : point_t -> float
val z : point_t -> float

(** Sum of two points *)
val sum : point_t -> point_t -> point_t

(** Subtraction of two points *)
val sub : point_t -> point_t -> point_t

(** Product between point and scalar value *)
val prod : point_t -> float -> point_t

(** Division between point and scalar value.
    Returns [None] if [num] is 0 *)
val div : point_t -> float -> point_t option
val from_coords : float -> float -> float -> point_t
val string_of_point : point_t -> string
val eq : point_t -> point_t -> bool
val ( + ) : point_t -> point_t -> point_t
val ( - ) : point_t -> point_t -> point_t
val distance : point_t -> point_t -> float

(**
    Given a list of points return the mean on each axis of all of them
    Returns [None] if the list is empty
*)
val mean : point_t list -> point_t option 
