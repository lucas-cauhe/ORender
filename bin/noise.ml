open Computer_gfx.Colorspace
open Computer_gfx.Common
module PpmDb = Computer_gfx.Db.Ppm

let usage_msg = "nosie -in <noisy_img> -ref <ref_img>"
let noisy_img = ref "ppms/rendered/cornell_tonemapped.ppm"
let ref_img = ref "ppms/rendered/512_rpp_reference.ppm"

let speclist =
  [ "-in", Arg.Set_string noisy_img, "Image to compute noise from"
  ; "-ref", Arg.Set_string ref_img, "Reference image"
  ]
;;

let rmse sq_error num_items = sqrt @@ (sq_error /. num_items)

let () =
  let () = Arg.parse speclist (fun _ -> ()) usage_msg in
  let noisy_ic = open_in !noisy_img in
  let ref_ic = open_in !ref_img in
  let noisy_header = PpmDb.read_header noisy_ic in
  let ref_header = PpmDb.read_header ref_ic in
  let rec traverse_file noise num_items =
    match PpmDb.read_pixel noisy_ic noisy_header with
    | None ->
      close_in noisy_ic;
      close_in ref_ic;
      rmse noise (float_of_int num_items)
    | Some p ->
      (match PpmDb.read_pixel ref_ic ref_header with
       | Some ref_p ->
         traverse_file
           (noise +. Rgb.sum_inside (Rgb.sub p ref_p) |> square)
           (num_items + 1)
       | None -> 0.)
  in
  let noise_result = traverse_file 0. 0 in
  Printf.printf "Computed noise -> %f\n" noise_result
;;
