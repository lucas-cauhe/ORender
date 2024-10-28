type bvh_algorithm = LargestAxis | Sah
val split_scene : Figures.scene -> bvh_algorithm -> Figures.scene