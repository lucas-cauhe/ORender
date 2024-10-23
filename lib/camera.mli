
type camera
type light_source_type

val camera : 
    Geometry.Direction.t ->
    Geometry.Direction.t ->
    Geometry.Direction.t ->
    Geometry.Point.t ->
    (int * int) -> 
    camera

val light_source : Geometry.Point.t -> Colorspace.Rgb.pixel -> light_source_type

val pixel_color : camera -> int * int -> Figures.scene -> light_source_type list -> Domainslib.Task.pool -> Colorspace.Rgb.pixel