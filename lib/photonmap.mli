module PhotonMap : module type of Kdtree.Make(Photon.Photon) 
val photonmap : Scene.Figures.scene -> Scene.Light.light_source list -> PhotonMap.t -> Scene.Figures.ray_type -> Colorspace.Rgb.pixel
val random_walk : Scene.Figures.scene -> Scene.Light.light_source list -> int -> PhotonMap.t