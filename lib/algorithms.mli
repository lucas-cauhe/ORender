
type algorithm = Photonmap | Pathtracing

val photonmap_pixel_color: Scene.Camera.camera -> int * int -> Scene.Light.light_source list -> Scene.Figures.scene -> Photonmap.PhotonMap.t -> Domainslib.Task.pool -> Common.texture_map -> Colorspace.Rgb.pixel 

val pathtracing_pixel_color : Scene.Camera.camera -> int * int -> Scene.Figures.scene ->  Scene.Light.light_source list -> Domainslib.Task.pool -> Common.texture_map -> Colorspace.Rgb.pixel 

val color_image : algorithm -> Scene.Camera.camera -> out_channel -> Db.Ppm.config -> Scene.Figures.scene -> Common.texture_map -> Scene.Light.light_source list -> int -> int -> unit