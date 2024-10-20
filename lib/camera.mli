
type camera

val camera : 
    Geometry.Direction.t ->
    Geometry.Direction.t ->
    Geometry.Direction.t ->
    Geometry.Point.t ->
    (int * int) -> 
    camera

val pixel_color : camera -> int * int -> Figures.scene -> Domainslib.Task.pool -> Colorspace.Rgb.pixel