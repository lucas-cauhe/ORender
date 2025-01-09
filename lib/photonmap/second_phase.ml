open Scene
open Geometry
open Pathtracing.Bindings
open Colorspace
module PhotonMap = First_phase.PhotonMap

let impossible_ls =
  Light.light_source
    (Light.Point (Point.from_coords 0. 0. 0.))
    (Rgb.rgb_of_values 0. 0. 0.)
;;

(** Search the k nearest photons within a search radius inside the given photonmap *)
let photon_search
  (photonmap : PhotonMap.t)
  (point : Point.point_t)
  (radius : float)
  (surface : Figures.figure)
  : Photon.t list * float
  =
  let knn, radius = PhotonMap.nearest_neighbors photonmap (Photon.point point) radius in
  (* BatList.take 1000 knn, radius *)
  List.filter (Photon.in_surface surface) knn |> BatList.take 1000, radius
;;

(** Compute densitiy estimation for a list of photons

    [density_estimation brdf_fun kernel_fun photons] returns the resulting pixel of computing the passed in [brdf_fun] and [kernel_fun] on each individual photon to obtain the global density estimation *)
let density_estimation (brdf : Photon.t -> Rgb.pixel) (kernel : Kernels.kernel_type)
  : Photon.t list -> Rgb.pixel
  =
  let photon_acc_sum acc photon =
    Rgb.value_prod (Photon.flux photon) (Kernels.kernel_fun photon kernel)
    |> Rgb.rgb_prod (brdf photon)
    |> Rgb.sum acc
  in
  List.fold_left photon_acc_sum (Rgb.rgb_of_values 0. 0. 0.)
;;

let rec photonmap scene ls pmap texture_map wi =
  let& fig, ir = Pathtracing.trace_ray scene wi, impossible_ls in
  let (material, prob), is_delta = Figures.get_figure fig |> Brdf.is_delta in
  if is_delta then (
    let outgoing_direction =
      Brdf.montecarlo_sample (Figures.get_figure fig) ir wi.ray_direction material
    in
    photonmap
      scene
      ls
      pmap
      texture_map
      (Figures.ray ir.intersection_point outgoing_direction)
  ) else (
    let knn, knn_radius =
      photon_search pmap ir.intersection_point 0.1 (Figures.get_figure fig)
    in
    let global_light_contribution =
      density_estimation
        (Photon.brdf
           (Figures.get_figure fig)
           ir.surface_normal
           wi.ray_direction
           (Diffuse, prob))
        (* (Gaussian { intersection_position = ir.intersection_point; smooth = 0.5 }) *)
        (Kernels.Box knn_radius)
        knn
    in
    Rgb.rgb_prod
      (Figures.get_figure fig |> Figures.emission ir.intersection_point texture_map)
      global_light_contribution
  )
;;

let rec nee_photonmap scene ls photonmap texture_map wi =
  let& fig, ir = Pathtracing.trace_ray scene wi, impossible_ls in
  let (material, prob), is_delta = Figures.get_figure fig |> Brdf.is_delta in
  if is_delta then (
    let outgoing_direction =
      Brdf.montecarlo_sample (Figures.get_figure fig) ir wi.ray_direction material
    in
    nee_photonmap
      scene
      ls
      photonmap
      texture_map
      (Figures.ray ir.intersection_point outgoing_direction)
  ) else (
    let knn, knn_radius =
      photon_search photonmap ir.intersection_point 0.1 (Figures.get_figure fig)
    in
    let diffuse_brdf =
      Brdf.brdf
        (Figures.get_figure fig)
        ir.surface_normal
        wi.ray_direction
        (Direction.from_coords 0. 0. 0.) (* Not used for computing diffuse brdf *)
        (Diffuse, prob)
    in
    let direct_light_contribution = Pathtracing.direct_light scene ls ir diffuse_brdf in
    let global_light_contribution =
      density_estimation
        (Photon.brdf
           (Figures.get_figure fig)
           ir.surface_normal
           wi.ray_direction
           (Diffuse, prob))
        (* (Gaussian { intersection_position = ir.intersection_point; smooth = 0.5 }) *)
        (Box knn_radius)
        knn
    in
    Rgb.sum direct_light_contribution global_light_contribution
    |> Rgb.rgb_prod
         (Figures.get_figure fig |> Figures.emission ir.intersection_point texture_map)
  )
;;
