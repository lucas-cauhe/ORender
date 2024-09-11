open Computer_gfx.Geometry
open Alcotest


let vector = 
  let module M = struct
    type t = vector
    let pp fmt p = Format.fprintf fmt "%s" (to_string_vector p)
    let equal = (=)
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)
let test_sum _ =
  
  let p1: vector = Point{x = 1.0; y = 2.0; z = 3.0} in
  let p2: vector = Point{x = 4.0; y = 5.0; z = 6.0} in
  let result: vector option = sum p1 p2 in
  let expected: vector = Point {x = 5.0; y = 7.0; z = 9.0} in
  match result with
  | Some(v) -> check vector "sum" expected v
  | _ -> raise Test_error
let suite =
  [
    "test_sum", `Quick, test_sum;
  ]

let () =
  Alcotest.run "Geometry" [ "Sum", suite ]