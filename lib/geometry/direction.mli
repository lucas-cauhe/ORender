(** Geometry's Direction module *)

type direction_t
(** Direction internal type *)

val x : direction_t -> float
(** Access the x axis component of the direction *)

val y : direction_t -> float
(** Access the y axis component of the direction *)

val z : direction_t -> float
(** Access the z axis component of the direction *)

val sum : direction_t -> direction_t -> direction_t
(** Sum two directions *)

val sub : direction_t -> direction_t -> direction_t
(** Subtract one direction from another *)

val prod : direction_t -> float -> direction_t
(** Scale a direction by a factor *)

val div : direction_t -> float -> direction_t option
(** Divide a direction by a factor

    Returns [None] in case the factor is [0]
*)

val dot : direction_t -> direction_t -> float
(** Dot product *)

val modulus : direction_t -> float
(** Modulus operation for a given direction *)

val normalize : direction_t -> direction_t option
(** Normalizes a direction

    Returns [None] if the direction is [(0,0,0)]
*)

val cross_product : direction_t -> direction_t -> direction_t
(** Cross product between two directions *)

val from_coords : float -> float -> float -> direction_t
(** Build a direction given each axis *)

val of_point : Point.point_t -> direction_t
(** Build a direction from a given point *)

val between_points : Point.point_t -> Point.point_t -> direction_t
(** Computes the direction from one [point] to another *)


val string_of_direction : direction_t -> string
(** Prettify a direction *)

val eq : direction_t -> direction_t -> bool
(** Test if two directions are the same *)

val angle : direction_t -> direction_t -> float
(** Compute the angle formed between two directions *)

val inv : direction_t -> direction_t
(** Invert a direction *)

val perpendicular : direction_t -> direction_t
(** Compute a perpendicular direction to one given  *)

val ( /* ) : direction_t -> direction_t -> direction_t
val ( * ) : direction_t -> direction_t -> float
