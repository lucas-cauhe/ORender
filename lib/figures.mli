type ray_type = {
  ray_origin: Geometry.Point.t;
  ray_direction: Geometry.Direction.t
}
type figure
type scene = figure list

val point_of_ray : ray_type -> float -> Geometry.Point.t

type intersection = {
  distance: float;
  surface_normal: Geometry.Direction.t
}
type intersection_result = Zero | Intersects of intersection list
val intersects : figure -> ray_type -> intersection_result
val show_figure : figure -> unit

val plane : Geometry.Direction.t -> Geometry.Point.t -> Colorspace.Rgb.pixel -> figure
val sphere : Geometry.Point.t -> float -> Colorspace.Rgb.pixel -> figure
val triangle : Geometry.Point.t -> Geometry.Point.t -> Geometry.Point.t -> Colorspace.Rgb.pixel -> figure option

val find_closest_figure : scene -> ray_type -> figure option

val emission : figure -> Colorspace.Rgb.pixel
