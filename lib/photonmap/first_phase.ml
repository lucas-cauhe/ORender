open Colorspace
open Domainslib
open Geometry
open Scene
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
  let photon_ray = Photon.to_ray current_photon in
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
           (Figures.get_figure fig)
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
       let incident_photon =
         Photon.photon
           (Photon.flux current_photon)
           ir.intersection_point
           outgoing_direction
           (Figures.get_figure fig)
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
         Photon.photon
           photon_radiance
           ir.intersection_point
           outgoing_direction
           (Figures.get_figure fig)
       in
       if is_first_photon then
         scatter_photons scene light next_photon photons false
       else
         scatter_photons scene light next_photon (photons @ [ incident_photon ]) false)
  | _ -> photons
;;

let random_walk scene light_sources num_random_walks pool =
  let scene_lights_weights = weight_scene_lights light_sources num_random_walks in
  let internal_random_walk ind =
    let next_light, emitted_photons = scene_lights_weights.(ind) in
    let init_direction =
      Brdf.sample_spherical_direction_solid
        (Direction.from_coords 0. (-1.) 0.)
        (Light.sample_light_point next_light)
    in
    let next_photon_flux =
      Rgb.value_prod
        (Light.power next_light)
        (4. *. Float.pi /. float_of_int emitted_photons)
    in
    let initial_photon =
      Photon.photon
        next_photon_flux
        (Light.sample_light_point next_light)
        init_direction
        (Figures.empty ())
    in
    scatter_photons scene next_light initial_photon [] false
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
  let scattered_photons = Task.run pool (fun _ -> scene_photons ()) in
  Printf.printf "Number of total photons: %d\n" (List.length scattered_photons);
  PhotonMap.make scattered_photons
;;
