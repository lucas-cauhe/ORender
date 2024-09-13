(** This module tests every geometry operation*)
open Computer_gfx.Geometry
open Alcotest

(** Testable instances for Point and Direction*)
let testable_point = 
  let module M = struct
    type t = Point.t
    let pp fmt p = Format.fprintf fmt "%s" (Point.string_of_point p)
    let equal = Point.eq
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)


let testable_direction = 
  let module M = struct
    type t = Direction.t
    let pp fmt p = Format.fprintf fmt "%s" (Direction.string_of_direction p)
    let equal = Direction.eq
  end in
  (module M : Alcotest.TESTABLE with type t = M.t) 

let testable_float = 
  let module M = struct
    type t = float
    let pp fmt p = Format.fprintf fmt "%f" p
    let equal = (=)
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

(** Tests for Point and Direction operations*)
let test_sum_p _ =
  let p1: Point.t = Point.from_coords 1.0 2.0 3.0 in
  let p2: Point.t = Point.from_coords 4.0 5.0 6.0 in
  let result: Point.t = Point.sum p1 p2 in
  let expected: Point.t = Point.from_coords 5.0 7.0 9.0 in
  check testable_point "sum" expected result

let test_sum_dir _ =
  let d1: Direction.t = Direction.from_coords 1.0 2.0 3.0 in
  let d2: Direction.t = Direction.from_coords 4.0 5.0 6.0 in
  let result: Direction.t = Direction.sum d1 d2 in
  let expected: Direction.t = Direction.from_coords 5.0 7.0 9.0 in
  check testable_direction "sum" expected result

let test_sub_p _ =
  let p1: Point.t = Point.from_coords 1.0 2.0 3.0 in
  let p2: Point.t = Point.from_coords 4.0 5.0 6.0 in
  let result: Point.t = Point.sub p1 p2 in
  let expected: Point.t = Point.from_coords (-3.0) (-3.0) (-3.0) in
  check testable_point "sub" expected result

let test_sub_dir _ =
  let d1: Direction.t = Direction.from_coords 1.0 2.0 3.0 in
  let d2: Direction.t = Direction.from_coords 4.0 5.0 6.0 in
  let result: Direction.t = Direction.sub d1 d2 in
  let expected: Direction.t = Direction.from_coords (-3.0) (-3.0) (-3.0) in
  check testable_direction "sub" expected result

let test_prod_p _ =
  let p: Point.t = Point.from_coords 1.0 2.0 3.0 in
  let num: float = 2.0 in
  let result: Point.t = Point.prod p num in
  let expected: Point.t = Point.from_coords 2.0 4.0 6.0 in
  check testable_point "prod" expected result

let test_prod_dir _ =
  let d: Direction.t = Direction.from_coords 1.0 2.0 3.0 in
  let num: float = 2.0 in
  let result: Direction.t = Direction.prod d num in
  let expected: Direction.t = Direction.from_coords 2.0 4.0 6.0 in
  check testable_direction "prod" expected result

let test_div_p _ =
  let p: Point.t = Point.from_coords 1.0 2.0 3.0 in
  let num: float = 2.0 in
  let result: Point.t option = Point.div p num in
  let expected: Point.t option = Some(Point.from_coords 0.5 1.0 1.5) in
  check (option testable_point) "div" expected result

let test_div_0 _ =
  let p: Point.t = Point.from_coords 1.0 2.0 3.0 in
  let num: float = 0.0 in
  let result: Point.t option = Point.div p num in
  let expected: Point.t option = None in
  check (option testable_point) "div" expected result

let test_div_dir _ =
  let d: Direction.t = Direction.from_coords 1.0 2.0 3.0 in
  let num: float = 2.0 in
  let result: Direction.t option = Direction.div d num in
  let expected: Direction.t option = Some(Direction.from_coords 0.5 1.0 1.5) in
  check (option testable_direction) "div" expected result

let test_dot _ =
  let d1: Direction.t = Direction.from_coords 1.0 2.0 3.0 in
  let d2: Direction.t = Direction.from_coords 4.0 5.0 6.0 in
  let result: float = Direction.dot d1 d2 in
  let expected: float = 32.0 in
  check testable_float "dot" expected result

let test_modulus _ =
  let d: Direction.t = Direction.from_coords 1.0 2.0 3.0 in
  let result: float = Direction.modulus d in
  let expected: float = sqrt(14.0) in
  check testable_float "modulus" expected result

let test_normalize _ =
  let d: Direction.t = Direction.from_coords 1.0 2.0 3.0 in
  let result: Direction.t option = Direction.normalize d in
  let expected: Direction.t option = Some(Direction.from_coords (1.0 /. sqrt(14.0)) (2.0 /. sqrt(14.0)) (3.0 /. sqrt(14.0))) in
  check (option testable_direction) "normalize" expected result

let test_cross_product _ =
  let d1: Direction.t = Direction.from_coords 1.0 2.0 3.0 in
  let d2: Direction.t = Direction.from_coords 4.0 5.0 6.0 in
  let result: Direction.t = Direction.cross_product d1 d2 in
  let expected: Direction.t = Direction.from_coords (-3.0) 6.0 (-3.0) in
  check testable_direction "cross_product" expected result

let test_sum =
  [
    "test_sum_p", `Quick, test_sum_p;
    "test_sum_dir", `Quick, test_sum_dir;
  ]

let test_sub =
  [
    "test_sub_p", `Quick, test_sub_p;
    "test_sub_dir", `Quick, test_sub_dir;
  ]

let test_prod =
  [
    "test_prod_p", `Quick, test_prod_p;
    "test_prod_dir", `Quick, test_prod_dir;
  ]

let test_div =
  [
    "test_div_p", `Quick, test_div_p;
    "test_div_0", `Quick, test_div_0;
    "test_div_dir", `Quick, test_div_dir;
  ]

let test_dot =
  [
    "test_dot", `Quick, test_dot;
  ]

let test_modulus =
  [
    "test_modulus", `Quick, test_modulus;
  ]

let test_normalize =
  [
    "test_normalize", `Quick, test_normalize;
  ]

let test_cross_product =
  [
    "test_cross_product", `Quick, test_cross_product;
  ]

let () =
  Alcotest.run "Geometry" [ "Sum", test_sum; "Sub", test_sub; "Prod", test_prod; "Div", test_div; "Dot", test_dot; "Modulus", test_modulus; "Normalize", test_normalize; "Cross Product", test_cross_product ]