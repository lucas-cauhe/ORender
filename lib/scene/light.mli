(**
  Type that represents a light source
*)
type light_source

(**
  Type of light source: [Area] or [Point]
*)
type light_source_type = 
  Area of Figures.scene_figure | Point of Geometry.Point.point_t

(**
    Return a [light_source] value given its center and power
*)
val light_source : light_source_type -> Colorspace.Rgb.pixel -> light_source

(**
  Get the type of light source
*)
val light_source_type_val : light_source -> light_source_type

(**
  Get the power of the light source
*)
val power : light_source -> Colorspace.Rgb.pixel

(**
  Returns the power of the given light source that affects the point represented in the intersection.
  If there is a figure between the intersection point and the light source, then the power is 0 otherwise
  the original power is scaled by the distance from the light source's center to the intersection point
  It will return the original power wihtout scaling it if the intersection point is the light source's center
*)
val shadow_ray : Figures.scene -> Figures.intersection -> light_source -> Colorspace.Rgb.pixel

(**
  Check whether a [Point.point_t] is placed in the surface of an area light source.
  If the given [light_source] is a point light, result will be [false]
*)
val point_belongs_to_ls : Geometry.Point.point_t -> light_source -> bool