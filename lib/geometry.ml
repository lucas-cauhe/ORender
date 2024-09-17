type axis = X | Y | Z

module Point = struct
  
  type t = {
    x: float;
    y: float;
    z: float;
  }

  let x p = p.x
  let y p = p.y
  let z p = p.z
  
  let sum p1 p2 = {x = p1.x +. p2.x; y = p1.y +. p2.y; z = p1.z +. p2.z}

  let sub p1 p2 =  {x = p1.x -. p2.x; y = p1.y -. p2.y; z = p1.z -. p2.z}

  let prod p num = {x = p.x *. num; y = p.y *. num; z = p.z *. num}

  let div p = function 0. -> None 
  | num -> Some({x = p.x /. num; y = p.y /. num; z = p.z /. num})

  let from_coords x y z = { x; y; z }
  let string_of_point p = Printf.sprintf "Point {x = %f; y = %f; z = %f}" p.x p.y p.z
  let eq p1 p2 = p1.x = p2.x && p1.y = p2.y && p1.z = p2.z
end

module Direction = struct
  type t = {
    x: float;
    y: float;
    z: float;
  }
  let x d = d.x
  let y d = d.y
  let z d = d.z
  let sum d1 d2 = {x = d1.x +. d2.x; y = d1.y +. d2.y; z = d1.z +. d2.z}

  let sub d1 d2 =  {x = d1.x -. d2.x; y = d1.y -. d2.y; z = d1.z -. d2.z}

  let prod d num = {x = d.x *. num; y = d.y *. num; z = d.z *. num}

  let div d = function 0. -> None 
  | num -> Some({x = d.x /. num; y = d.y /. num; z = d.z /. num})

  let dot d1 d2 = d1.x *. d2.x +. d1.y *. d2.y +. d1.z *. d2.z
  let modulus d = sqrt (d.x *. d.x +. d.y *. d.y +. d.z *. d.z)

  let normalize d = modulus d |> div d

  let cross_product d1 d2 = { x = d1.y *. d2.z -. d1.z *. d2.y; y = d1.z *. d2.x -. d1.x *. d2.z; z = d1.x *. d2.y -. d1.y *. d2.x }

  let from_coords x y z = { x; y; z }
  let string_of_direction d = Printf.sprintf "Direction {x = %f; y = %f; z = %f}" d.x d.y d.z
  let eq d1 d2 = d1.x = d2.x && d1.y = d2.y && d1.z = d2.z
end

module Matrix = struct
  type t = float array array

  let identity dim = Array.init_matrix dim dim (fun i j -> if i = j then 1. else 0.)

  let get_element m i j = m.(i).(j)

  let transpose m = 
    let dimy = Array.length m in
    let dimx = Array.length m.(0) in
    Array.init_matrix dimx dimy (fun i j -> m.(j).(i))

  let from_array_matrix mat = 
    let dimy = Array.length mat in
    let dimx = Array.length mat.(0) in
    Array.init_matrix dimy dimx (fun i j -> mat.(i).(j))

  let string_of_row = Array.fold_left (fun acc el -> acc ^ (Printf.sprintf "%f " el ) ) ""
  
  let string_of_matrix = 
    Array.fold_left (fun result nextRow -> result ^ "| " ^ (string_of_row nextRow) ^ "|\n") ""

  let multiply m1 m2 =
    if (Array.length m1.(0)) <> (Array.length m2) then
      None
    else
      let m' = Array.make_matrix (Array.length m1) (Array.length m2.(0)) 0.0 in
      for i = 0 to 3 do
        for j = 0 to 3 do
          for k = 0 to 3 do
            m'.(i).(j) <- m'.(i).(j) +. m1.(i).(k) *. m2.(k).(j)
          done
        done
      done;
      Some(m')
end


  
module Transformations = struct
  type hc = Point of Point.t | Direction of Direction.t

  let prod (homCoord : hc) = function
  [ x; y; z ] -> begin
    match homCoord with 
    | Point(p) -> 
      let p' = Point.from_coords (Point.x p |> ( *. ) x) (Point.y p |> ( *. ) y) (Point.z p |> ( *. ) z) in
      Point(p')
    | Direction(d) ->
      let d' = Direction.from_coords (Direction.x d |> ( *. ) x) (Direction.y d |> ( *. ) y) (Direction.z d |> ( *. ) z) in
      Direction(d')
  end
  | _ -> homCoord

  let hc_of_point p = Point(p)
  let hc_of_direction d = Direction(d)
  let translate mat = function
  Point(p) -> begin
    let tx = Matrix.get_element mat 0 3 in
    let ty = Matrix.get_element mat 1 3 in
    let tz = Matrix.get_element mat 2 3 in
    let lastColPoint = Point.from_coords tx ty tz in
    let translationHc = Point.sum lastColPoint p in
    Some(Point( translationHc ))
  end
  | _ -> None
  let scale mat homCoord = 
    let sx = Matrix.get_element mat 0 0 in
    let sy = Matrix.get_element mat 1 1 in
    let sz = Matrix.get_element mat 2 2 in
    prod homCoord [ sx; sy; sz ]
  let rotate mat axis homCoord = match axis, homCoord with
    | X, Point(p) -> 
      let rotatedY = (Point.z p) *. (Matrix.get_element mat 1 2) |> (-.) ((Point.y p) *. (Matrix.get_element mat 1 1)) in 
      let rotatedZ = (Point.z p) *. (Matrix.get_element mat 2 2) |> (+.) ((Point.y p) *. (Matrix.get_element mat 2 1)) in
      let rotatedPoint = Point.from_coords (Point.x p) rotatedY rotatedZ in
      Point(rotatedPoint)
    | Y, Point(p) ->
      let rotatedX = (Point.x p) *. (Matrix.get_element mat 0 0) |> (+.) ((Point.z p) *. (Matrix.get_element mat 0 2)) in 
      let rotatedZ = (Point.x p) *. (Matrix.get_element mat 2 0) |> (-.) ((Point.z p) *. (Matrix.get_element mat 2 2)) in
      let rotatedPoint = Point.from_coords rotatedX (Point.y p) rotatedZ in
      Point(rotatedPoint)
    | Z, Point(p) -> 
      let rotatedX = (Point.y p) *. (Matrix.get_element mat 0 1) |> (-.) ((Point.x p) *. (Matrix.get_element mat 0 0)) in 
      let rotatedY = (Point.y p) *. (Matrix.get_element mat 1 1) |> (+.) ((Point.x p) *. (Matrix.get_element mat 1 0)) in
      let rotatedPoint = Point.from_coords rotatedX rotatedY (Point.z p) in
      Point(rotatedPoint)
    | X, Direction(d) -> 
      let rotatedY = (Direction.z d) *. (Matrix.get_element mat 1 2) |> (-.) ((Direction.y d) *. (Matrix.get_element mat 1 1)) in 
      let rotatedZ = (Direction.z d) *. (Matrix.get_element mat 2 2) |> (-.) ((Direction.y d) *. (Matrix.get_element mat 2 1)) in
      let rotatedDirection = Direction.from_coords (Direction.x d) rotatedY rotatedZ in
      Direction(rotatedDirection)
    | Y, Direction(d) ->
      let rotatedX = (Direction.x d) *. (Matrix.get_element mat 0 0) |> (+.) ((Direction.z d) *. (Matrix.get_element mat 0 2)) in 
      let rotatedZ = (Direction.x d) *. (Matrix.get_element mat 2 0) |> (-.) ((Direction.z d) *. (Matrix.get_element mat 2 2)) in
      let rotatedDirection = Direction.from_coords rotatedX (Direction.y d) rotatedZ in
      Direction(rotatedDirection)
    | Z, Direction(d) -> 
      let rotatedX = (Direction.y d) *. (Matrix.get_element mat 0 1) |> (-.) ((Direction.x d) *. (Matrix.get_element mat 0 0)) in 
      let rotatedY = (Direction.y d) *. (Matrix.get_element mat 1 1) |> (+.) ((Direction.x d) *. (Matrix.get_element mat 1 0)) in
      let rotatedDirection = Direction.from_coords rotatedX rotatedY (Direction.z d) in
      Direction(rotatedDirection)
end

  
