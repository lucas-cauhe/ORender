
type camera

val camera : 
    Geometry.Direction.t ->
    Geometry.Direction.t ->
    Geometry.Direction.t ->
    Geometry.Point.t ->
    camera

val trace_ray : camera -> int * int -> Figures.scene -> Domainslib.Task.pool -> Colorspace.Rgb.pixel