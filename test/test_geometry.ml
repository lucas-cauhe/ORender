open Computer_gfx.Geometry
open Alcotest


let testable_point = 
  let module M = struct
    type t = Point.t
    let pp fmt p = Format.fprintf fmt "%s" (Point.string_of_point p)
    let equal = Point.eq
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)
let test_sum _ =
  let p1: Point.t = Point.from_coords 1.0 2.0 3.0 in
  let p2: Point.t = Point.from_coords 4.0 5.0 6.0 in
  let result: Point.t = Point.sum p1 p2 in
  let expected: Point.t = Point.from_coords 5.0 7.0 9.0 in
  check testable_point "sum" expected result
let suite =
  [
    "test_sum", `Quick, test_sum;
  ]

let () =
  Alcotest.run "Geometry" [ "Sum", suite ]