(**
    Photonmap module holds functionality for the photonmapping algorithm. It supports both, pure and next-event estimation verions of the algorithm
*)

(**
    Representation of the PhotonMap using a Kdtree
*)
module PhotonMap : module type of Kdtree.Make(Photon) 

(**
    Pure photonmap implementation. Direct and global light are obtained from the photonmap

    [photonmap scene light_sources photons init_direction] computes the color of the resulting pixel for tracing the [init_direction] ray into the scene
*)
val photonmap : Scene.Figures.scene -> Scene.Light.light_source list -> PhotonMap.t -> Scene.Figures.ray_type -> Colorspace.Rgb.pixel

(**
    Photonmap implementation using next-event estimation. Direct light is computed using next-event estimation and global light is obtained from the photonmap

    [nee_photonmap scene light_sources photons init_direction] computes the color of the resulting pixel for tracing the [init_direction] ray into the scene
*)
val nee_photonmap : Scene.Figures.scene -> Scene.Light.light_source list -> PhotonMap.t -> Scene.Figures.ray_type -> Colorspace.Rgb.pixel

(**
    Build the photonmap

    [random_walk scene light_sources num_walks pool] returns a photonmap from the given [scene] and [light_sources] after scattering [num_walks] initial photons. 
    It contains the info needed to compute the [scene]'s illumination
*)
val random_walk : Scene.Figures.scene -> Scene.Light.light_source list -> int -> Domainslib.Task.pool -> PhotonMap.t 