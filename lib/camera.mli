
type camera

val camera : 
    Geometry.Direction.t ->
    Geometry.Direction.t ->
    Geometry.Direction.t ->
    Geometry.Point.t ->
    (int * int) -> 
    camera


(**
    Computes the color of a pixel using the Path Tracer algorithm
    Receives a [camera_type] camera, the pixel coordinates, the scene and light sources
*)
val pixel_color : camera -> int * int -> Figures.scene -> Light.light_source list -> Domainslib.Task.pool -> Colorspace.Rgb.pixel