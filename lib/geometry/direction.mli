(** Geometry's Direction module *)

type direction_t
(** Direction internal type *)

val x : direction_t -> float
val y : direction_t -> float
val z : direction_t -> float
val sum : direction_t -> direction_t -> direction_t
val sub : direction_t -> direction_t -> direction_t
val prod : direction_t -> float -> direction_t
val div : direction_t -> float -> direction_t option
val dot : direction_t -> direction_t -> float
val modulus : direction_t -> float
val normalize : direction_t -> direction_t option
val cross_product : direction_t -> direction_t -> direction_t
val from_coords : float -> float -> float -> direction_t
val of_point : Point.point_t -> direction_t
val between_points : Point.point_t -> Point.point_t -> direction_t
val string_of_direction : direction_t -> string
val eq : direction_t -> direction_t -> bool
val angle : direction_t -> direction_t -> float
val inv : direction_t -> direction_t
val perpendicular : direction_t -> direction_t
val ( /* ) : direction_t -> direction_t -> direction_t
val ( * ) : direction_t -> direction_t -> float
