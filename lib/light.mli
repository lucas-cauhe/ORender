(**
    Type that represents a light source
*)
type light_source

(**
    Return a [light_source] value given its center and power
*)
val light_source : Geometry.Point.t -> Colorspace.Rgb.pixel -> light_source

(**
  Returns the power of the given light source that affects the point represented in the intersection.
  If there is a figure between the intersection point and the light source, then the power is 0 otherwise
  the original power is scaled by the distance from the light source's center to the intersection point
  It will return the original power wihtout scaling it if the intersection point is the light source's center
*)
val shadow_ray : Figures.scene -> Figures.intersection -> light_source -> Colorspace.Rgb.pixel