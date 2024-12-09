(*
  Authors: Lucas Cauhé Viñao, Andrei Dumbrava
  Description: Figures interface
*)

(**
  Figures module defines computations over figure types and collections of figures
*)

type ray_type = {
  ray_origin : Geometry.Point.point_t;
  ray_direction : Geometry.Direction.direction_t;
}
(** Ray defined by its origin [ray_origin] and direction [ray_direction]
*)

type coefficients =
  Colorspace.Rgb.pixel * Colorspace.Rgb.pixel * Colorspace.Rgb.pixel

val ray : Geometry.Point.point_t -> Geometry.Direction.direction_t -> ray_type
val string_of_ray : ray_type -> string

type figure_properties =
  { emission : Colorspace.Rgb.pixel
  ; coefficients : coefficients
  ; refraction : float
  }

type figure
(**
  Every 3d figure that can be represented in a rendering scene
*)

(**
  Tree-like representation of the figures in the scene
*)
type scene_figure =
  | Figure of figure
  | BoundingBox of figure * scene_figure list

type scene = scene_figure list
(**
  Set of figures in a 3d space
*)

val scene_size : scene -> int
(**
  Returns the amount of figures in a scene
  A bounding box counts as 1 
*)

val point_of_ray : ray_type -> float -> Geometry.Point.point_t
(**
  Compute the point in the ray's direction from a given offset
*)

val dist_to_point_of_ray : ray_type -> Geometry.Point.point_t -> float
(**
  Compute the distance to the point contained in the ray's direction
*)

val get_figure : scene_figure -> figure
(**
  Utility function to retrieve the internal figure 
*)

type intersection = {
  distance : float;
  surface_normal : Geometry.Direction.direction_t;
  intersection_point : Geometry.Point.point_t;
}
(**
  Ray-figure intersection point info
*)

(**
  [Zero] if operation result yielded no ray-figure intersections
  [Intersects(il)] if operation yielded at least one intersection
*)
type intersection_result = Zero | Intersects of intersection list

val intersects : figure -> ray_type -> intersection_result
(**
  Whether a ray intersects with a figure
*)

val show_figure : figure -> unit
(**
  Figure pretty print
*)

val transform : Geometry.transformation -> figure -> figure option
(**
  Given a [transformation] geometry type and a figure to apply it to, returns a new [figure] result of transforming the figure given
*)

val vertices : figure -> Geometry.Point.point_t list
(**
  Function to retrieve the vertices of a figure
*)

val barycenter : figure -> Geometry.Point.point_t
(**
  Function to retrieve the barycenter of a figure
*)

val point_belongs_to_fig : Geometry.Point.point_t -> figure -> bool
(**
  Test whether a point belongs to a figure
*)

val plane :
  Geometry.Direction.direction_t ->
  Geometry.Point.point_t ->
  figure_properties ->
  figure
(**
  Returns the instance of a plane given its [normal] and a [point] in the plane
*)

val sphere :
  Geometry.Point.point_t ->
  float ->
  figure_properties ->
  figure
(**
  Returns the instance of a sphere given its [center] and [radius] of the sphere 
*)

val triangle :
  Geometry.Point.point_t ->
  Geometry.Point.point_t ->
  Geometry.Point.point_t ->
    figure_properties ->
  figure option
(**
  Returns [Some(t)] with the instance of a triangle given three points if a triangle can be formed out of them 
  Returns [None] otherwise
*)

val cuboid :
  Geometry.Point.point_t ->
  Geometry.Point.point_t ->
    figure_properties ->
  figure
(**
  Returns the instance of a cuboid given its [min_point] and [max_point] defining the cuboid 
*)

val empty : unit -> figure
(**
  Returns an [Empty] figure, used for failing computations or debugging purposes
*)

val bounding_box : figure -> scene_figure list -> scene_figure option
(**
  Returns a BoundingBox Figure type from a cuboid and a list of child nodes
  Returns [None] if the figure argument is not a cuboid type
*)

val find_closest_figure :
  scene -> ray_type -> (scene_figure * intersection_result) option
(**
  Given a [scene] and a [ray] returns [Some(figure, intersection_result)] with the closest intersected figure in the scene by the ray
  Returns [None] if no figure was intersected 
*)

val emission : figure -> Colorspace.Rgb.pixel
val coefficients : figure -> coefficients
val refraction : figure -> float
val is_sphere : scene_figure -> bool
val is_plane : scene_figure -> bool
val is_same_figure : figure -> figure -> bool
val rotate_figure : scene_figure -> Geometry.Matrix.matrix_t -> Geometry.axis -> scene_figure 
val translate_figure : float -> float -> float -> scene_figure -> scene_figure 
val scale_figure : float -> float -> float -> scene_figure -> scene_figure 