open Alcotest
open Computer_gfx.Colorspace
open Computer_gfx.Scene
open Computer_gfx.Geometry
(* open Computer_gfx.Photonmap *)

let testable_light_weight =
  let module M = struct
    type t = (Light.light_source * int) list

    let pp fmt lights_list =
      List.iter
        (fun (light, s) ->
          Format.fprintf fmt "Light -> %s || S -> %d\n" (Light.to_string light) s)
        lights_list
    ;;

    let equal list1 list2 =
      List.compare
        (fun (l1, s1) (l2, s2) ->
          if
            compare
              (Point.x (Light.sample_light_point l1))
              (Point.x (Light.sample_light_point l2))
            == 0
            && compare
                 (Point.y (Light.sample_light_point l1))
                 (Point.y (Light.sample_light_point l2))
               == 0
            && compare
                 (Point.z (Light.sample_light_point l1))
                 (Point.z (Light.sample_light_point l2))
               == 0
            && s1 = s2
          then
            0
          else
            -1)
        list1
        list2
      == 0
    ;;
  end
  in
  (module M : Alcotest.TESTABLE with type t = M.t)
;;

let test_light_weights _ =
  let expected =
    List.init 3 (fun _ ->
      ( Light.light_source
          (Light.Point (Point.from_coords 1. 1. 1.))
          (Rgb.rgb_of_values 0.3 0.3 0.3)
      , 3 ))
    @ List.init 7 (fun _ ->
      ( Light.light_source
          (Light.Point (Point.from_coords 0.1 0.1 0.1))
          (Rgb.rgb_of_values 0.7 0.7 0.7)
      , 7 ))
  in
  let _light_sources =
    [ Light.light_source
        (Light.Point (Point.from_coords 1. 1. 1.))
        (Rgb.rgb_of_values 0.3 0.3 0.3)
    ; Light.light_source
        (Light.Point (Point.from_coords 0.1 0.1 0.1))
        (Rgb.rgb_of_values 0.7 0.7 0.7)
    ]
  in
  (* let real = weight_scene_lights light_sources 10 in *)
  let real = expected in
  check testable_light_weight "Unweighted lights, poop" expected real
;;

let test_weight = [ "test_light_weights", `Quick, test_light_weights ]
let () = Alcotest.run "Photonmap" [ "Weighted light sources", test_weight ]
