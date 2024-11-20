type light_source
(**
  Type that represents a light source
*)

(**
  Type of light source: [Area] or [Point]
*)
type light_source_type =
  | Area of Figures.scene_figure
  | Point of Geometry.Point.point_t

val light_source : light_source_type -> Colorspace.Rgb.pixel -> light_source
(**
    Return a [light_source] value given its center and power
*)

val light_source_type_val : light_source -> light_source_type
(**
  Get the type of light source
*)

val power : light_source -> Colorspace.Rgb.pixel
(**
  Get the power of the light source
*)

val shadow_ray :
  Figures.scene -> Figures.intersection -> light_source -> Colorspace.Rgb.pixel
(**
  Returns the power of the given light source that affects the point represented in the intersection.
  If there is a figure between the intersection point and the light source, then the power is 0 otherwise
  the original power is scaled by the distance from the light source's center to the intersection point
  It will return the original power wihtout scaling it if the intersection point is the light source's center
*)

val point_belongs_to_ls : Geometry.Point.point_t -> light_source -> bool
(**
  Check whether a [Point.point_t] is placed in the surface of an area light source.
  If the given [light_source] is a point light, result will be [false]
*)

(**
  Samples lights for a given list of [light_source]
  If the light_source list is [Point] light, then a new one is returned like the given one
  If the light_source list is [Area] light, then a random one is selected (if multiple) and multiple [Point] light are sampled from it
*)
val sample_light : light_source list -> light_source list

(* val produce_lights : light_source list -> light_source   *)
(**
  Creates an (infinite) list of photons weighted by the contribution of 
  each light source in the scene 
*)

val sample_light_point : light_source -> Geometry.Point.point_t

val sample_light_source : light_source -> light_source

val to_string : light_source -> string