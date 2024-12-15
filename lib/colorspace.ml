module type ColorSpace = sig
  type pixel

  val luminance : pixel -> float
  val merge_chans : pixel -> float -> float -> pixel
  val equalized : float -> pixel
  val show : pixel -> string
  val eq : pixel -> pixel -> bool
end

module type CsConversor = sig
  include ColorSpace

  val pixel_of_rgb : pixel -> pixel
  val rgb_of_pixel : pixel -> pixel
end

module Rgb = struct
  type pixel =
    { red : float
    ; green : float
    ; blue : float
    }

  let luminance p = (0.2126 *. p.red) +. (0.7152 *. p.green) +. (0.0722 *. p.blue)

  let merge_chans p l_in l_out =
    { red = p.red *. (l_out /. l_in)
    ; green = p.green *. (l_out /. l_in)
    ; blue = p.blue *. (l_out /. l_in)
    }
  ;;

  let equalized v = { red = v; green = v; blue = v }

  let rescale_pixel p base scale =
    { red = p.red *. scale /. base
    ; green = p.green *. scale /. base
    ; blue = p.blue *. scale /. base
    }
  ;;

  let rgb_of_values r g b = { red = r; green = g; blue = b }
  let red p = p.red
  let green p = p.green
  let blue p = p.blue
  let pixel_of_rgb p = p
  let rgb_of_pixel p = p
  let show p = Printf.sprintf "(%f, %f, %f)" p.red p.green p.blue
  let eq p1 p2 = p1.red == p2.red && p1.green == p2.green && p1.blue == p2.blue

  let sum p1 p2 =
    { red = p1.red +. p2.red; green = p1.green +. p2.green; blue = p1.blue +. p2.blue }
  ;;

  let normalize p norm =
    { red = p.red /. norm; green = p.green /. norm; blue = p.blue /. norm }
  ;;

  let value_prod p factor =
    { red = p.red *. factor; green = p.green *. factor; blue = p.blue *. factor }
  ;;

  let rgb_prod p1 p2 =
    { red = p1.red *. p2.red; green = p1.green *. p2.green; blue = p1.blue *. p2.blue }
  ;;

  let zero () = { red = 0.; green = 0.; blue = 0. }

  let max p =
    if p.red >= p.green && p.red >= p.blue then
      p.red
    else if p.green >= p.red && p.green >= p.blue then
      p.green
    else
      p.blue
  ;;

  let sum_inside p = p.red +. p.green +. p.blue

  let abs_diff p1 p2 =
    (abs_float @@ (p1.red -. p2.red))
    +. (abs_float @@ (p1.green -. p2.green))
    +. (abs_float @@ (p1.blue -. p2.blue))
  ;;
end
