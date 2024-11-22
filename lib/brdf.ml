open Scene.Figures
open Colorspace
open Geometry

type russian_roulette_result =
  | Absorption
  | Diffuse
  | Specular
  | Refraction

let absorption_prob = ref 0.2

let delta wr wi =
  if Direction.cross_product wr wi |> Direction.modulus < 0.001 then
    1.
  else
    0.
;;

let sample_specular wo normal =
  Direction.dot wo normal
  |> ( *. ) 2.
  |> Direction.prod normal
  |> Direction.sub wo
  |> Direction.normalize
  |> Option.get
;;

let sample_refraction w0 normal media_in media_out =
  let _theta_zero = Direction.angle normal w0 in
  let entrance = Direction.dot w0 normal < 0. in
  let media_out =
    if entrance then
      media_out
    else
      1.
  in
  let inv_normal =
    if entrance then
      normal
    else
      Direction.inv normal
  in
  let ratio =
    if entrance then
      1. /. media_out
    else
      media_in
  in
  let cosTh = min 1. (Direction.dot (Direction.inv w0) normal) in
  let sinTh = sqrt (1. -. Common.square cosTh) in
  (* let theta_critical = Float.asin (media_out /. media_in) in *)
  if ratio *. sinTh > 1. then
    sample_specular w0 inv_normal, media_out
  else (
    (* let incident = Direction.prod w0 (media_in /. media_out) in
       let thetai = Float.asin (media_in /. media_out *. Float.sin theta_zero) in
       let out =
       Direction.prod inv_normal ((media_in /. media_out *. cos theta_zero) -. cos thetai)
       in *)
    let mycos = min 1. (Direction.dot inv_normal (Direction.inv w0)) in
    let rPerp =
      Direction.prod (Direction.prod inv_normal mycos |> Direction.sum w0) ratio
    in
    let rPar =
      Direction.dot rPerp rPerp
      |> ( -. ) 1.
      |> abs_float
      |> sqrt
      |> Direction.prod (Direction.inv inv_normal)
    in
    (* Direction.sum incident out, media_out *)
    Direction.sum rPerp rPar, media_out
  )
;;

let sample_spherical_direction normal origin =
  let rand_lat = 1. -. Random.float 1. |> sqrt |> Float.acos in
  let rand_azimut = 2. *. Float.pi *. Random.float 1. in
  (* It is impossible that the modulus of the global coords' direction is ever 0, hence can do Option.get *)
  let normal_normalized = Direction.normalize normal |> Option.get in
  let wi_prime =
    Geometry.cartesian_of_spherical rand_lat rand_azimut normal_normalized
    |> Geometry.Transformations.hc_of_direction
  in
  let cb_mat = Geometry.cb_matrix_tangent normal_normalized origin in
  Geometry.Transformations.change_basis cb_mat wi_prime
  |> Geometry.Transformations.direction_of_hc
;;

let montecarlo_sample
  fig
  { surface_normal = normal; intersection_point = ip; _ }
  wo
  scene_media
  = function
  | Diffuse -> sample_spherical_direction normal ip, scene_media
  | Specular -> sample_specular wo normal, scene_media
  | Refraction ->
    let media_out = refraction fig in
    sample_refraction wo normal scene_media media_out
  | Absorption -> normal, scene_media
;;

let random_choice (k1_type, k1) (k2_type, k2) =
  let p1 = Rgb.max k1 /. (Rgb.max k1 +. Rgb.max k2) in
  let p2 = 1. -. p1 in
  if Random.float 1. <= p1 then
    k1_type, p1
  else
    k2_type, p2
;;

let only_choice kd ks _ =
  let ki_type =
    if Rgb.max kd > 0. then
      Diffuse
    else if Rgb.max ks > 0. then
      Specular
    else
      Refraction
  in
  ki_type, 1. -. !absorption_prob
;;

let russian_roulette (fig : figure) =
  let kd, ks, kt = coefficients fig in
  let choice =
    if Rgb.max kd > 0. && Rgb.max ks > 0. then
      random_choice (Diffuse, kd) (Specular, ks)
    else if Rgb.max ks > 0. && Rgb.max kt > 0. then
      random_choice (Specular, ks) (Refraction, kt)
    else
      only_choice kd ks kt
  in
  if Random.float 1. < !absorption_prob then
    Absorption, !absorption_prob
  else
    choice
;;

let brdf fig n w0 wi (rres, prob) scene_media =
  let kd, ks, kt = coefficients fig in
  match rres with
  | Absorption -> Rgb.zero ()
  | Diffuse -> Rgb.normalize kd prob (* uniform cosine sampling *)
  | Specular ->
    let wr = sample_specular w0 n in
    Rgb.normalize (Rgb.value_prod ks (delta wr wi)) (Direction.dot n wi *. prob)
  | Refraction ->
    let media_out = refraction fig in
    let wr, _ = sample_refraction w0 n scene_media media_out in
    Rgb.normalize (Rgb.value_prod kt (delta wr wi)) (Direction.dot n wi *. prob)
;;

let cosine_norm (n : Direction.direction_t) (wi : Direction.direction_t) =
  Direction.dot n wi |> abs_float
;;
