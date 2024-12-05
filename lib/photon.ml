open Colorspace
open Scene
open Geometry

module Photon = struct
  let dim = 3

  type real = float
  type point = Point.point_t

  let point p = p

  type t =
    { flux : Rgb.pixel
    ; position : Point.point_t
    ; direction : Direction.direction_t
    ; surface : Figures.figure
    }

  let photon flux position direction surface = { flux; position; direction; surface }
  let position ph = ph.position
  let direction ph = ph.direction
  let flux ph = ph.flux
  let surface ph = ph.surface

  let to_string ph =
    Printf.sprintf
      "Flux -> %s | Position -> %s | Direction -> %s\n"
      (Rgb.show ph.flux)
      (Point.string_of_point ph.position)
      (Direction.string_of_direction ph.direction)
  ;;

  (* unused *)
  type range = (float * float) array

  let axial_compare n p1 p2 =
    match n with
    | 0 -> compare (Point.x p1) (Point.x p2)
    | 1 -> compare (Point.y p1) (Point.y p2)
    | 2 -> compare (Point.z p1) (Point.z p2)
    | _ -> invalid_arg @@ "Expected axis 0, 1 or 2, found " ^ string_of_int n
  ;;

  let to_point { position = p; _ } = p
  let null_interval = max_float, min_float
  let null_range = Array.make dim null_interval

  let add_loc ((x0, x1) as interval) x =
    if x0 < x1 then
      if x < x0 then
        x, x1
      else if x > x1 then
        x0, x
      else
        interval
    else if x0 = x1 then
      if x > x0 then
        x0, x
      else if x < x0 then
        x, x0
      else
        interval
    else
      (* x0 > x1, invalid range *)
      x, x
  ;;

  let range_maker r { position = p; _ } =
    let mapf i interval =
      if i = 0 then
        add_loc interval (Point.x p)
      else
        add_loc interval (Point.y p)
    in
    Array.mapi mapf r
  ;;

  let squared_distance p1 p2 = Geometry.Point.distance p1 p2 |> Common.square

  let squared_axial_distance n p1 p2 =
    match n with
    | 0 -> Point.x p1 -. Point.x p2 |> Common.square
    | 1 -> Point.y p1 -. Point.y p2 |> Common.square
    | 2 -> Point.z p1 -. Point.z p2 |> Common.square
    | _ -> invalid_arg @@ "Expected axis 0, 1 or 2, found " ^ string_of_int n
  ;;

  (* BAD IMPLEMENTED BECAUSE IT'S UNUSED *)
  let point_in_range r p =
    let x0, x1 = Array.get r 0 in
    let y0, y1 = Array.get r 1 in
    Point.x p >= x0 && Point.x p <= x1 && Point.y p >= y0 && Point.y p <= y1
  ;;

  let is_valid_interval (x0, x1) = x0 <= x1

  let intersect_intervals (x00, x01) (x10, x11) =
    if x01 < x10 || x11 < x00 then
      null_interval
    else
      max x00 x10, min x01 x11
  ;;

  let intersect_ranges r0 r1 =
    let x0_interval = Array.get r0 0 in
    let y0_interval = Array.get r0 1 in
    let x1_interval = Array.get r1 0 in
    let y1_interval = Array.get r1 1 in
    let ivi = is_valid_interval in
    if ivi x0_interval && ivi x1_interval && ivi y0_interval && ivi y1_interval then (
      let x_interval = intersect_intervals x0_interval x1_interval in
      let y_interval = intersect_intervals y0_interval y1_interval in
      if ivi x_interval && ivi y_interval then
        [| x_interval; y_interval |]
      else
        null_range
    ) else
      null_range
  ;;
end
