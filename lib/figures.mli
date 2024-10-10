type ray_type = {
  ray_origin: Geometry.Point.t;
  ray_direction: Geometry.Direction.t
}
type figure
type scene = figure list

val point_of_ray : ray_type -> float -> Geometry.Point.t

val intersects : figure -> ray_type -> float list option
val show_figure : figure -> unit

val plane : Geometry.Direction.t -> Geometry.Point.t -> figure
val sphere : Geometry.Point.t -> float -> figure
val triangle : Geometry.Point.t -> Geometry.Point.t -> Geometry.Point.t -> figure option

val find_closest_figure : scene -> ray_type -> figure option
