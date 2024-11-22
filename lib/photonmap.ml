open Scene
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
;;

let rec scatter_photons scene light current_photon (photons : Photon.t list) =
  let photon_ray =
    Figures.ray (Photon.position current_photon) (Photon.direction current_photon)
  in
  match Pathtracing.trace_ray scene photon_ray with
  | _, Figures.Zero -> photons
  | fig, Figures.Intersects (ir :: _) ->
    (match Brdf.russian_roulette (Figures.get_figure fig) with
     | Absorption, _ -> photons
     | roulette_result, roulette_prob ->
       let outgoing_direction, _ =
         Brdf.montecarlo_sample
           (Figures.get_figure fig)
           ir
           photon_ray.ray_direction
           1.
           roulette_result
       in
       let current_brdf =
         Brdf.brdf
           (Figures.get_figure fig)
           ir.surface_normal
           photon_ray.ray_direction
           outgoing_direction
           (roulette_result, roulette_prob)
           1.
       in
       let brdf_cosine =
         Rgb.value_prod
           current_brdf
           (Brdf.cosine_norm ir.surface_normal outgoing_direction)
       in
       let photon_radiance = Rgb.rgb_prod brdf_cosine (Light.power light) in
       let next_photon =
         Photon.photon photon_radiance ir.intersection_point outgoing_direction
       in
       scatter_photons scene light next_photon (photons @ [ next_photon ]))
  | _ -> photons
;;

let random_walk scene light_sources num_random_walks =
  let scene_lights_weights = weight_scene_lights light_sources num_random_walks in
  let rec rec_random_walk photons = function
    | [] -> photons
    | (next_light, emitted_photons) :: rest_lights ->
      let init_direction =
        Brdf.sample_spherical_direction
          (Direction.from_coords 0. 1. 0.)
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
      in
      let current_light_photons = scatter_photons scene next_light initial_photon [] in
      rec_random_walk (photons @ current_light_photons) rest_lights
  in
  let scene_photons = rec_random_walk [] scene_lights_weights |> Array.of_list in
  PhotonMap.create scene_photons
;;