(** Associate functions to work with 3D points *)

type point_t
(** Point internal type *)

val x : point_t -> float
val y : point_t -> float
val z : point_t -> float

val sum : point_t -> point_t -> point_t
(** Sum of two points *)

val sub : point_t -> point_t -> point_t
(** Subtraction of two points *)

val prod : point_t -> float -> point_t
(** Product between point and scalar value *)

val div : point_t -> float -> point_t option
(** Division between point and scalar value.
    Returns [None] if [num] is 0 *)

val from_coords : float -> float -> float -> point_t
val string_of_point : point_t -> string
val eq : point_t -> point_t -> bool
val ( + ) : point_t -> point_t -> point_t
val ( - ) : point_t -> point_t -> point_t
val distance : point_t -> point_t -> float

val mean : point_t list -> point_t option
(**
    Given a list of points return the mean on each axis of all of them
    Returns [None] if the list is empty
*)
