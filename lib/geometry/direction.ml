type direction_t =
  { x : float
  ; y : float
  ; z : float
  }

let x d = d.x
let y d = d.y
let z d = d.z
let sum d1 d2 = { x = d1.x +. d2.x; y = d1.y +. d2.y; z = d1.z +. d2.z }
let sub d1 d2 = { x = d1.x -. d2.x; y = d1.y -. d2.y; z = d1.z -. d2.z }
let prod d num = { x = d.x *. num; y = d.y *. num; z = d.z *. num }

let div d = function
  | 0. -> None
  | num -> Some { x = d.x /. num; y = d.y /. num; z = d.z /. num }
;;

let dot d1 d2 =
  let dot_ = (d1.x *. d2.x) +. (d1.y *. d2.y) +. (d1.z *. d2.z) in
  if abs_float dot_ <= 1e-10 then
    0.
  else
    dot_
;;

let modulus d = sqrt ((d.x *. d.x) +. (d.y *. d.y) +. (d.z *. d.z))
let normalize d = modulus d |> div d
let of_point p = { x = Point.x p; y = Point.y p; z = Point.z p }
let between_points p1 p2 = Point.sub p1 p2 |> of_point

let cross_product d1 d2 =
  { x = (d1.y *. d2.z) -. (d1.z *. d2.y)
  ; y = (d1.z *. d2.x) -. (d1.x *. d2.z)
  ; z = (d1.x *. d2.y) -. (d1.y *. d2.x)
  }
;;

let from_coords x y z = { x; y; z }

let string_of_direction d =
  Printf.sprintf "Direction {x = %f; y = %f; z = %f}" d.x d.y d.z
;;

let eq d1 d2 = d1.x = d2.x && d1.y = d2.y && d1.z = d2.z
let ( /* ) = cross_product
let ( * ) = dot

let perpendicular d =
  if d.z != 0. && -.d.x != d.y then
    from_coords d.z d.z (-.d.x -. d.y)
  else
    from_coords (d.y -. d.z) d.x d.x
;;

let angle d1 d2 = Float.acos @@ (dot d1 d2 /. modulus d1 /. modulus d2)
