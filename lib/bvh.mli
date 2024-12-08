(**
    Type of algorithm to use
*)
type bvh_algorithm = LargestAxis | Sah

(**
    Represent the given scene with all the primitives (except planes) wrapped in a bounding volume hierarchy
*)
val split_scene : Scene.Figures.scene -> bvh_algorithm -> Scene.Figures.scene