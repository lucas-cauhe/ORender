open Scene
open Domainslib
open Pathtracing.Bindings
open Geometry
open Photon

(* open Iter.Infix *)
open Colorspace
module PhotonMap = Kdtree.Make (Photon)

let weight_scene_lights ls num_photons =
  let total_power =
    List.fold_left
      (fun acc light_source -> Rgb.sum acc (Light.power light_source))
      (Rgb.zero ())
      ls
  in
  List.fold_left
    (fun acc light_source ->
      let s =
        float_of_int num_photons
        *. (Rgb.sum_inside (Light.power light_source) /. Rgb.sum_inside total_power)
        |> Float.round
        |> int_of_float
      in
      acc @ List.init s (fun _ -> Light.sample_light_source light_source, s))
    []
    ls
  |> Array.of_list
;;

let rec scatter_photons
  scene
  light
  current_photon
  (photons : Photon.t list)
  is_first_photon
  =
  let photon_ray =
    Figures.ray (Photon.position current_photon) (Photon.direction current_photon)
  in
  match Pathtracing.trace_ray scene photon_ray with
  | _, Figures.Zero -> photons
  | fig, Figures.Intersects (ir :: _) ->
    (match Brdf.russian_roulette (Figures.get_figure fig) with
     | Absorption, _ -> photons
     | ((Specular | Refraction) as roulette_result), _ ->
       let outgoing_direction =
         Brdf.montecarlo_sample
           (Figures.get_figure fig)
           ir
           photon_ray.ray_direction
           roulette_result
       in
       let next_photon =
         Photon.photon
           (Photon.flux current_photon)
           ir.intersection_point
           outgoing_direction
       in
       scatter_photons scene light next_photon photons false
     | Diffuse, roulette_prob ->
       let outgoing_direction =
         Brdf.montecarlo_sample
           (Figures.get_figure fig)
           ir
           photon_ray.ray_direction
           Diffuse
       in
       let current_brdf =
         Brdf.brdf
           (Figures.get_figure fig)
           ir.surface_normal
           photon_ray.ray_direction
           outgoing_direction
           (Diffuse, roulette_prob)
       in
       let brdf_cosine =
         Rgb.value_prod
           current_brdf
           (Brdf.cosine_norm ir.surface_normal outgoing_direction)
       in
       let photon_radiance = Rgb.rgb_prod brdf_cosine (Photon.flux current_photon) in
       let next_photon =
         Photon.photon photon_radiance ir.intersection_point outgoing_direction
       in
       if is_first_photon then
         scatter_photons scene light next_photon photons false
       else
         scatter_photons scene light next_photon (photons @ [ next_photon ]) false)
  | _ -> photons
;;

let random_walk scene light_sources num_random_walks pool =
  let scene_lights_weights = weight_scene_lights light_sources num_random_walks in
  let internal_random_walk ind =
    let next_light, emitted_photons = scene_lights_weights.(ind) in
    let init_direction =
      Brdf.sample_spherical_direction
        (Direction.from_coords (-1.) 0. 0.)
        (Light.sample_light_point next_light)
    in
    let next_photon_flux =
      Rgb.value_prod
        (Light.power next_light)
        (4. *. Float.pi /. float_of_int emitted_photons)
    in
    let initial_photon =
      Photon.photon next_photon_flux (Light.sample_light_point next_light) init_direction
    in
    scatter_photons scene next_light initial_photon [] true
  in
  let scene_photons () =
    Task.parallel_for_reduce
      ~start:0
      ~finish:(Array.length scene_lights_weights - 1)
      ~body:internal_random_walk
      pool
      (fun acc photons -> acc @ photons)
      []
  in
  PhotonMap.make (Task.run pool (fun _ -> scene_photons ()))
;;

let impossible_ls =
  Light.light_source
    (Light.Point (Geometry.Point.from_coords 0. 0. 0.))
    (Rgb.rgb_of_values 0. 0. 0.)
;;

(* let photon_search (photonmap : Photon.t list) (point : Geometry.Point.point_t) (k : int)
  : Photon.t list
  =
  let rec search_nearest nearest photons =
    match photons with
    | [] -> nearest
    | photon :: rest_photons ->
      let dist_to_point =
        Direction.between_points point (Photon.position photon) |> Direction.modulus
      in
      let current_max, curr_max_ind, _ =
        List.fold_left
          (fun (acc, max_ind, curr_ind) p ->
            let curr_dist =
              Direction.between_points point (Photon.position p) |> Direction.modulus
            in
            if acc > curr_dist then
              acc, max_ind, curr_ind + 1
            else
              curr_dist, curr_ind, curr_ind + 1)
          (0., 0, 0)
          nearest
      in
      if current_max > dist_to_point then
        search_nearest
          (List.init curr_max_ind (fun i -> List.nth nearest i)
           @ [ photon ]
           @ List.init
               (k - 1 - curr_max_ind)
               (fun i -> List.nth nearest (curr_max_ind + 1 + i)))
          rest_photons
      else
        search_nearest nearest rest_photons
  in
  search_nearest (List.init k (fun i -> List.nth photonmap i)) photonmap
;; *)

let photon_search (photonmap : PhotonMap.t) (point : Geometry.Point.point_t)
  : Photon.t list * float
  =
  let knn, radius = PhotonMap.nearest_neighbors photonmap (Photon.point point) in
  (* BatList.take 3 knn, radius *)
  knn, radius
;;

let photon_brdf
  ((fig, ir) : Figures.scene_figure * Figures.intersection)
  (wi : Figures.ray_type)
  ((roulette_result, roulette_prob) : Brdf.russian_roulette_result * float)
  (photon : Photon.t)
  =
  Brdf.brdf
    (Figures.get_figure fig)
    ir.surface_normal
    wi.ray_direction
    (Photon.direction photon)
    (roulette_result, roulette_prob)
;;

type gaussian_kernel =
  { intersection_position : Geometry.Point.point_t
  ; smooth : float
  }

type kernel_type =
  | Box of float
  | Gaussian of gaussian_kernel

let _build_box = Box 1.

let _build_gaussian =
  Gaussian { intersection_position = Point.from_coords 0. 0. 0.; smooth = 0.5 }
;;

let kernel_fun (photon : Photon.t) = function
  | Box radius -> 1. /. Float.pi /. radius
  | Gaussian { intersection_position; smooth } ->
    (Float.exp
     @@ -.Common.square
            ((Direction.between_points intersection_position (Photon.position photon)
              |> Direction.modulus)
             /. smooth))
    /. Float.pi
    /. Common.square smooth
;;

let density_estimation (brdf : Photon.t -> Rgb.pixel) (kernel : kernel_type)
  : Photon.t list -> Rgb.pixel
  =
  let photon_acc_sum acc photon =
    Rgb.value_prod (Photon.flux photon) (kernel_fun photon kernel)
    |> Rgb.rgb_prod (brdf photon)
    |> Rgb.sum acc
  in
  List.fold_left photon_acc_sum (Rgb.rgb_of_values 0. 0. 0.)
;;

let rec rec_photonmap scene ls photonmap wi =
  let& ((fig, ir) as inter_params) = Pathtracing.trace_ray scene wi, impossible_ls in
  let (material, prob), is_delta = Figures.get_figure fig |> Brdf.is_delta in
  if is_delta then (
    let outgoing_direction =
      Brdf.montecarlo_sample (Figures.get_figure fig) ir wi.ray_direction material
    in
    rec_photonmap
      scene
      ls
      photonmap
      (Figures.ray ir.intersection_point outgoing_direction)
  ) else (
    let knn, knn_radius = photon_search photonmap ir.intersection_point in
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
        (photon_brdf inter_params wi (Diffuse, prob))
        (* (Gaussian { intersection_position = ir.intersection_point; smooth = 0.5 }) *)
        (Box knn_radius)
        knn
    in
    Rgb.sum direct_light_contribution global_light_contribution
    |> Rgb.rgb_prod (Figures.get_figure fig |> Figures.emission)
  )
;;
