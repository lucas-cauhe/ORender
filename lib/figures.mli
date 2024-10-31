(*
  Authors: Lucas Cauhé Viñao, Andrei Dumbrava
  Description: Figures interface
*)

(**
  Figures module defines computations over figure types and collections of figures
*)

(** Ray defined by its origin [ray_origin] and direction [ray_direction]
*)
type ray_type = {
  ray_origin: Geometry.Point.t;
  ray_direction: Geometry.Direction.t
}

val ray : Geometry.Point.t -> Geometry.Direction.t -> ray_type
val string_of_ray : ray_type -> string 

(**
  Every 3d figure that can be represented in a rendering scene
*)
type figure

(**
  Tree-like representation of the figures in the scene
*)
type scene_figure = Figure of figure | BoundingBox of figure * scene_figure list 

(**
  Set of figures in a 3d space
*)
type scene = scene_figure list

(**
  Returns the amount of figures in a scene
  A bounding box counts as 1 
*)
val scene_size : scene -> int 

(**
  Compute the point in the ray's direction from a given offset
*)
val point_of_ray : ray_type -> float -> Geometry.Point.t

(**
  Compute the distance to the point contained in the ray's direction
*)
val dist_to_point_of_ray : ray_type -> Geometry.Point.t -> float

(**
  Utility function to retrieve the internal figure 
*)
val get_figure : scene_figure -> figure


(**
  Ray-figure intersection point info
*)
type intersection = {
  distance: float;
  surface_normal: Geometry.Direction.t;
  intersection_point: Geometry.Point.t;
}

(**
  [Zero] if operation result yielded no ray-figure intersections
  [Intersects(il)] if operation yielded at least one intersection
*)
type intersection_result = Zero | Intersects of intersection list

(**
  Whether a ray intersects with a figure
*)
val intersects : figure -> ray_type -> intersection_result

(**
  Figure pretty print
*)
val show_figure : figure -> unit

(**
  Given a [transformation] geometry type and a figure to apply it to, returns a new [figure] result of transforming the figure given
*)
val transform : Geometry.transformation -> figure -> figure option

(**
  Function to retrieve the vertices of a figure
*)
val vertices : figure -> Geometry.Point.t list

(**
  Function to retrieve the barycenter of a figure
*)
val barycenter : figure -> Geometry.Point.t

(**
  Returns the instance of a plane given its [normal] and a [point] in the plane
*)
val plane : Geometry.Direction.t -> Geometry.Point.t -> Colorspace.Rgb.pixel -> coefficients:Colorspace.Rgb.pixel -> figure

(**
  Returns the instance of a sphere given its [center] and [radius] of the sphere 
*)
val sphere : Geometry.Point.t -> float -> Colorspace.Rgb.pixel -> coefficients:Colorspace.Rgb.pixel -> figure

(**
  Returns [Some(t)] with the instance of a triangle given three points if a triangle can be formed out of them 
  Returns [None] otherwise
*)
val triangle : Geometry.Point.t -> Geometry.Point.t -> Geometry.Point.t -> Colorspace.Rgb.pixel -> coefficients:Colorspace.Rgb.pixel -> figure option

(**
  Returns the instance of a cuboid given its [min_point] and [max_point] defining the cuboid 
*)
val cuboid : Geometry.Point.t -> Geometry.Point.t -> Colorspace.Rgb.pixel -> coefficients:Colorspace.Rgb.pixel -> figure

(**
  Returns an [Empty] figure, used for failing computations or debugging purposes
*)
val empty : unit -> figure

(**
  Returns a BoundingBox Figure type from a cuboid and a list of child nodes
  Returns [None] if the figure argument is not a cuboid type
*)
val bounding_box : figure -> scene_figure list -> scene_figure option

(**
  Given a [scene] and a [ray] returns [Some(figure, intersection_result)] with the closest intersected figure in the scene by the ray
  Returns [None] if no figure was intersected 
*)
val find_closest_figure : scene -> ray_type -> (scene_figure * intersection_result) option

val emission : figure -> Colorspace.Rgb.pixel
val brdf : figure -> Geometry.Direction.t -> Geometry.Direction.t -> float 

val is_sphere : scene_figure -> bool
val is_plane : scene_figure -> bool
