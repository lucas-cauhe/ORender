module PhotonMap : module type of Kdtree.Make(Photon.Photon) 
(* val rec_photonmap : Scene.Figures.scene -> Scene.Light.light_source list -> Photon.Photon.t list -> Scene.Figures.ray_type -> Colorspace.Rgb.pixel
val random_walk : Scene.Figures.scene -> Scene.Light.light_source list -> int -> Photon.Photon.t list  *)
val rec_photonmap : Scene.Figures.scene -> Scene.Light.light_source list -> PhotonMap.t -> Scene.Figures.ray_type -> Colorspace.Rgb.pixel
val random_walk : Scene.Figures.scene -> Scene.Light.light_source list -> int -> PhotonMap.t 