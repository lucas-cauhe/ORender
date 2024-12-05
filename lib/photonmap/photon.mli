(**
  Photon module defines the [photon] representation in a scene and for later use in the photonmapping algorithm
*)

include Kdtree.Multidim

(**
  [photon rgb_pixel source_point init_direction figure_surface] creates a new [Photon.t] photon 
*)
  val photon : Colorspace.Rgb.pixel -> Geometry.Point.point_t -> Geometry.Direction.direction_t -> Scene.Figures.figure ->  t

(**
  [to_ray photon] creates a ray from the given [photon]   
*)
val to_ray : t -> Scene.Figures.ray_type

(**
  [brdf figure surface_normal ray_direction (roulette_result, roulette_prob) photon] computes the brdf function for the given photon 
*)
val brdf : Scene.Figures.figure -> Geometry.Direction.direction_t -> Geometry.Direction.direction_t -> Brdf.russian_roulette_result * float -> t -> Colorspace.Rgb.pixel

(**
  [in_surface fig photon] checks whether the give [photon] belongs to [fig]'s surface 
*)
val in_surface : Scene.Figures.figure -> t -> bool

(**
  [direction_to_photon point photon] computes the direction between a [point] and a given [photon]
*)
val direction_to_point : Geometry.Point.point_t -> t -> Geometry.Direction.direction_t

(**
  [flux photon] returns the flux of the given [photon]
*)
val flux : t -> Colorspace.Rgb.pixel

(**
  [to_string photon] returns a formatted string with the [photon]'s info
*)
val to_string : t -> string
  val point : Geometry.Point.point_t -> point