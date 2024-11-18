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
  let theta_zero = Direction.angle normal w0 in
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
  let theta_critical = Float.asin @@ (media_out /. media_in) in
  if theta_zero >= theta_critical then
    sample_specular w0 inv_normal, media_out
  else (
    let incident = Direction.prod w0 (media_in /. media_out) in
    let thetai = Float.asin @@ (media_in /. media_out *. Float.sin theta_zero) in
    let out =
      Direction.prod inv_normal ((media_in /. media_out *. cos theta_zero) -. cos thetai)
    in
    Direction.sum incident out, media_out
  )
;;

let montecarlo_sample
  fig
  { surface_normal = normal; intersection_point = ip; _ }
  wo
  scene_media
  = function
  | Diffuse ->
    let rand_lat = 1. -. Random.float 1. |> sqrt |> Float.acos in
    let rand_azimut = 2. *. Float.pi *. Random.float 1. in
    (* It is impossible that the modulus of the global coords' direction is ever 0, hence can do Option.get *)
    let normal_normalized = Direction.normalize normal |> Option.get in
    let wi_prime =
      Geometry.cartesian_of_spherical rand_lat rand_azimut normal_normalized
      |> Geometry.Transformations.hc_of_direction
    in
    let cb_mat = Geometry.cb_matrix_tangent normal_normalized ip in
    ( Geometry.Transformations.change_basis cb_mat wi_prime
      |> Geometry.Transformations.direction_of_hc
    , scene_media )
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
    Rgb.normalize (Rgb.value_prod ks (delta wr wi)) (Direction.dot n wi)
  | Refraction ->
    let media_out = refraction fig in
    let wr, _ = sample_refraction w0 n scene_media media_out in
    Rgb.normalize (Rgb.value_prod kt (delta wr wi)) (Direction.dot n wi)
;;
