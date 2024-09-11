type point = {
  x: float;
  y: float;
  z: float;
}


type direction = {
  x: float;
  y: float;
  z: float;
}

type vector = 
  | Point of point
  | Direction of direction

let equal_vector (v1: vector) (v2: vector) : bool = 
  match v1, v2 with
  | Point p1, Point p2 -> p1.x = p2.x && p1.y = p2.y && p1.z = p2.z
  | Direction d1, Direction d2 -> d1.x = d2.x && d1.y = d2.y && d1.z = d2.z
  | _ -> false

let to_string_vector (v: vector) : string =
  match v with
  | Point p -> Printf.sprintf "Point {x = %f; y = %f; z = %f}" p.x p.y p.z
  | Direction d -> Printf.sprintf "Direction {x = %f; y = %f; z = %f}" d.x d.y d.z

let sum (v1 : vector) (v2 : vector) : vector option = 
  match v1, v2 with
  | Point p1, Point p2 -> Some(Point {x = p1.x +. p2.x; y = p1.y +. p2.y; z = p1.z +. p2.z})
  | Direction d1, Direction d2 -> Some(Direction {x = d1.x +. d2.x; y = d1.y +. d2.y; z = d1.z +. d2.z})
  | _ -> None

let sub (v1: vector) (v2: vector) : vector option = 
  match v1, v2 with
  | Point p1, Point p2 -> Some(Point {x = p1.x -. p2.x; y = p1.y -. p2.y; z = p1.z -. p2.z})
  | Direction d1, Direction d2 -> Some(Direction {x = d1.x -. d2.x; y = d1.y -. d2.y; z = d1.z -. d2.z})
  | _ -> None

let prod (v: vector) num : vector = 
  match v with
  | Point p -> Point {x = p.x *. num; y = p.y *. num; z = p.z *. num}
  | Direction d -> Direction {x = d.x *. num; y = d.y *. num; z = d.z *. num}

let div (v: vector) num =
  match v, num with
  | _, 0. -> None
  | Point p, num -> Some ( Point {x = p.x /. num; y = p.y /. num; z = p.z /. num} )
  | Direction d, num -> Some ( Direction {x = d.x /. num; y = d.y /. num; z = d.z /. num} )

let dot (v1: vector) (v2: vector) : float option = 
  match v1, v2 with
  | Direction { x = dx1; y = dy1; z = dz1 }, Direction { x = dx2; y = dy2; z = dz2 } -> Some (dx1 *. dx2 +. dy1 *. dy2 +. dz1 *. dz2)
  | _, _ -> None

let cross_product (v1: vector) (v2: vector) : vector option = 
  match v1, v2 with
  | Direction { x = dx1; y = dy1; z = dz1 }, Direction { x = dx2; y = dy2; z = dz2 } -> Some (Direction { x = dy1 *. dz2 -. dz1 *. dy2; y = dz1 *. dx2 -. dx1 *. dz2; z = dx1 *. dy2 -. dy1 *. dx2 })
  | _, _ -> None

let modulus (v: vector) : float option = 
  match v with
  | Direction d -> Some (sqrt (d.x *. d.x +. d.y *. d.y +. d.z *. d.z))
  | _ -> None

let normalize (v: vector) : vector option = 
  match v with
  | Direction d -> Option.bind (modulus v) (fun modv -> Some (Direction {x = d.x /. modv; y = d.y /. modv; z = d.z /. modv}))
  | _ -> None

