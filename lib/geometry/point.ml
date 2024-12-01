type point_t =
  { x : float
  ; y : float
  ; z : float
  }

let x p = p.x
let y p = p.y
let z p = p.z
let sum p1 p2 = { x = p1.x +. p2.x; y = p1.y +. p2.y; z = p1.z +. p2.z }
let sub p1 p2 = { x = p1.x -. p2.x; y = p1.y -. p2.y; z = p1.z -. p2.z }
let prod p num = { x = p.x *. num; y = p.y *. num; z = p.z *. num }

let div p = function
  | 0. -> None
  | num -> Some { x = p.x /. num; y = p.y /. num; z = p.z /. num }
;;

let from_coords x y z = { x; y; z }
let string_of_point p = Printf.sprintf "Point {x = %f; y = %f; z = %f}" p.x p.y p.z
let eq p1 p2 = p1.x = p2.x && p1.y = p2.y && p1.z = p2.z
let ( + ) = sum
let ( - ) = sub

let distance p1 p2 =
  let dx = p1.x -. p2.x in
  let dy = p1.y -. p2.y in
  let dz = p1.z -. p2.z in
  sqrt (Common.square dx +. Common.square dy +. Common.square dz)
;;

let mean plist =
  let x_total, y_total, z_total =
    List.fold_left
      (fun (x_acc, y_acc, z_acc) p -> x_acc +. p.x, y_acc +. p.y, z_acc +. p.z)
      (0., 0., 0.)
      plist
  in
  List.length plist |> float_of_int |> div (from_coords x_total y_total z_total)
;;
