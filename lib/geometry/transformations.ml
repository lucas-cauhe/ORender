type hc =
  | Point of Point.point_t
  | Direction of Direction.direction_t

type axis =
  | X
  | Y
  | Z

exception Change_basis_error of (Matrix.matrix_t * hc * string)

let prod (homCoord : hc) = function
  | [ x; y; z ] ->
    (match homCoord with
     | Point p ->
       let p' =
         Point.from_coords
           (Point.x p |> ( *. ) x)
           (Point.y p |> ( *. ) y)
           (Point.z p |> ( *. ) z)
       in
       Point p'
     | Direction d ->
       let d' =
         Direction.from_coords
           (Direction.x d |> ( *. ) x)
           (Direction.y d |> ( *. ) y)
           (Direction.z d |> ( *. ) z)
       in
       Direction d')
  | _ -> homCoord
;;

let hc_of_point p = Point p

let point_of_hc = function
  | Point p -> p
  | Direction d -> Point.from_coords (Direction.x d) (Direction.y d) (Direction.z d)
;;

let hc_of_direction d = Direction d

let direction_of_hc = function
  | Direction d -> d
  | Point p -> Direction.of_point p
;;

let _string_of_hc = function
  | Direction d -> Direction.string_of_direction d
  | Point p -> Point.string_of_point p
;;

let translate mat = function
  | Point p ->
    let tx = Matrix.get_element mat 0 3 in
    let ty = Matrix.get_element mat 1 3 in
    let tz = Matrix.get_element mat 2 3 in
    let lastColPoint = Point.from_coords tx ty tz in
    let translationHc = Point.sum lastColPoint p in
    Some (Point translationHc)
  | _ -> None
;;

let translation_transformation_of_values tx ty tz =
  Matrix.from_array_matrix
    [| [| 1.; 0.; 0.; tx |]
     ; [| 0.; 1.; 0.; ty |]
     ; [| 0.; 0.; 1.; tz |]
     ; [| 0.; 0.; 0.; 1. |]
    |]
;;

let scale mat homCoord =
  let sx = Matrix.get_element mat 0 0 in
  let sy = Matrix.get_element mat 1 1 in
  let sz = Matrix.get_element mat 2 2 in
  (* Printf.printf "homCoord before: %s\n" (string_of_hc homCoord); *)
  let res = prod homCoord [ sx; sy; sz ] in
  (* Printf.printf "homCoord after: %s\n" (string_of_hc res); *)
  res
;;

let scale_transformation_of_values tx ty tz =
  Matrix.from_array_matrix
    [| [| tx; 0.; 0.; 0. |]
     ; [| 0.; ty; 0.; 0. |]
     ; [| 0.; 0.; tz; 0. |]
     ; [| 0.; 0.; 0.; 1. |]
    |]
;;

let rotation_transformation_of_axis ~angle = function
  | X ->
    Matrix.from_array_matrix
      [| [| 1.; 0.; 0.; 0. |]
       ; [| 0.; cos angle; -.sin angle; 0. |]
       ; [| 0.; sin angle; cos angle; 0. |]
       ; [| 0.; 0.; 0.; 1. |]
      |]
  | Y ->
    Matrix.from_array_matrix
      [| [| cos angle; 0.; sin angle; 0. |]
       ; [| 0.; 1.; 0.; 0. |]
       ; [| -.sin angle; 0.; cos angle; 0. |]
       ; [| 0.; 0.; 0.; 1. |]
      |]
  | Z ->
    Matrix.from_array_matrix
      [| [| cos angle; -.sin angle; 0.; 0. |]
       ; [| sin angle; cos angle; 0.; 0. |]
       ; [| 0.; 0.; 1.; 0. |]
       ; [| 0.; 0.; 0.; 1. |]
      |]
;;

let rotate mat axis homCoord =
  match axis, homCoord with
  | X, Point p ->
    let rotatedY =
      Point.z p *. Matrix.get_element mat 1 2
      |> ( +. ) (Point.y p *. Matrix.get_element mat 1 1)
    in
    let rotatedZ =
      Point.z p *. Matrix.get_element mat 2 2
      |> ( +. ) (Point.y p *. Matrix.get_element mat 2 1)
    in
    let rotatedPoint = Point.from_coords (Point.x p) rotatedY rotatedZ in
    Point rotatedPoint
  | Y, Point p ->
    (* let rotatedX =
       Point.x p *. Matrix.get_element mat 0 0
       |> ( +. ) (Point.y p *. Matrix.get_element mat 0 1)
       |> ( +. ) (Point.z p *. Matrix.get_element mat 0 2)
       |> ( +. ) (1. *. Matrix.get_element mat 0 3)
       in
       Printf.printf
       "rotatedX -> %f | Valores matrices 0 0 -> %f | Valores matrices 0 2 -> %f \n"
       rotatedX
       (Matrix.get_element mat 0 0)
       (Matrix.get_element mat 0 2);
       let rotatedZ =
       Point.x p *. Matrix.get_element mat 2 0
       |> ( +. ) (Point.y p *. Matrix.get_element mat 2 1)
       |> ( +. ) (Point.z p *. Matrix.get_element mat 2 2)
       |> ( +. ) (1. *. Matrix.get_element mat 2 3) *)
    (* let point_to_matrix p =
       Matrix.from_array_matrix [| [| Point.x p |]; [| Point.y p |]; [| Point.z p |]; [| 1.0 |] |] in
       let res = Matrix.multiply mat point_to_matrix in
       let point = Point.from_coords (Matrix.get_element res 0 0) (Matrix.get_element res 0 0) (Matrix.get_element res 0 0) in
       Point point
       in
       Printf.printf
       "rotatedZ -> %f | Valores matrices 2 0 -> %f | Valores matrices 2 2 -> %f \n"
       rotatedZ
       (Matrix.get_element mat 2 0)
       (Matrix.get_element mat 2 2);
       Printf.printf "Matrix of rotation:\n%s" (Matrix.string_of_matrix mat);
       Printf.printf
       "Before rotate x -> %f | y -> %f | z -> %f\n"
       (Point.x p)
       (Point.y p)
       (Point.z p);
       Printf.printf
       "After rotate x -> %f | y -> %f | z -> %f\n"
       rotatedX
       (Point.y p)
       rotatedZ;
       let rotatedPoint = Point.from_coords rotatedX (Point.y p) rotatedZ in
       Point rotatedPoint *)
    let point_to_matrix p =
      Matrix.from_array_matrix
        [| [| Point.x p |]; [| Point.y p |]; [| Point.z p |]; [| 1.0 |] |]
    in
    let res = Matrix.multiply mat (point_to_matrix p) |> Option.get in
    let point =
      Point.from_coords
        (Matrix.get_element res 0 0)
        (Matrix.get_element res 1 0)
        (Matrix.get_element res 2 0)
    in
    Point point
  | Z, Point p ->
    let rotatedX =
      Point.y p *. Matrix.get_element mat 0 1
      |> ( +. ) (Point.x p *. Matrix.get_element mat 0 0)
    in
    let rotatedY =
      Point.y p *. Matrix.get_element mat 1 1
      |> ( +. ) (Point.x p *. Matrix.get_element mat 1 0)
    in
    let rotatedPoint = Point.from_coords rotatedX rotatedY (Point.z p) in
    Point rotatedPoint
  | X, Direction d ->
    let rotatedY =
      Direction.z d *. Matrix.get_element mat 1 2
      |> ( -. ) (Direction.y d *. Matrix.get_element mat 1 1)
    in
    let rotatedZ =
      Direction.z d *. Matrix.get_element mat 2 2
      |> ( -. ) (Direction.y d *. Matrix.get_element mat 2 1)
    in
    let rotatedDirection = Direction.from_coords (Direction.x d) rotatedY rotatedZ in
    Direction rotatedDirection
  | Y, Direction d ->
    let rotatedX =
      Direction.x d *. Matrix.get_element mat 0 0
      |> ( +. ) (Direction.z d *. Matrix.get_element mat 0 2)
    in
    let rotatedZ =
      Direction.x d *. Matrix.get_element mat 2 0
      |> ( -. ) (Direction.z d *. Matrix.get_element mat 2 2)
    in
    let rotatedDirection = Direction.from_coords rotatedX (Direction.y d) rotatedZ in
    Direction rotatedDirection
  | Z, Direction d ->
    let rotatedX =
      Direction.y d *. Matrix.get_element mat 0 1
      |> ( -. ) (Direction.x d *. Matrix.get_element mat 0 0)
    in
    let rotatedY =
      Direction.y d *. Matrix.get_element mat 1 1
      |> ( +. ) (Direction.x d *. Matrix.get_element mat 1 0)
    in
    let rotatedDirection = Direction.from_coords rotatedX rotatedY (Direction.z d) in
    Direction rotatedDirection
;;

let change_basis mat = function
  | Point p as hcp ->
    let homCoordMatrix =
      Matrix.from_array_matrix
        [| [| Point.x p |]; [| Point.y p |]; [| Point.z p |]; [| 1. |] |]
    in
    let mresult = Matrix.multiply mat homCoordMatrix in
    (match mresult with
     | Some res ->
       Point
         (Point.from_coords
            (Matrix.get_element res 0 0)
            (Matrix.get_element res 1 0)
            (Matrix.get_element res 2 0))
     | None -> raise (Change_basis_error (mat, hcp, "Error changing base of point")))
  | Direction d as hcd ->
    let homCoordMatrix =
      Matrix.from_array_matrix
        [| [| Direction.x d |]; [| Direction.y d |]; [| Direction.z d |]; [| 0. |] |]
    in
    let mresult = Matrix.multiply mat homCoordMatrix in
    (match mresult with
     | Some res ->
       Direction
         (Direction.from_coords
            (Matrix.get_element res 0 0)
            (Matrix.get_element res 1 0)
            (Matrix.get_element res 2 0))
     | None -> raise (Change_basis_error (mat, hcd, "Error changing base of direction")))
;;

let cb_transformation_of_base d1 d2 d3 origin =
  let x = Direction.x d1
  and y = Direction.y d1
  and z = Direction.z d1 in
  let x' = Direction.x d2
  and y' = Direction.y d2
  and z' = Direction.z d2 in
  let x'' = Direction.x d3
  and y'' = Direction.y d3
  and z'' = Direction.z d3 in
  let o = Point.x origin
  and p = Point.y origin
  and q = Point.z origin in
  Matrix.from_array_matrix
    [| [| x; x'; x''; o |]
     ; [| y; y'; y''; p |]
     ; [| z; z'; z''; q |]
     ; [| 0.; 0.; 0.; 1. |]
    |]
;;

let combine_transformations initialHc = List.fold_left (fun accHc f -> f accHc) initialHc
