module type ColorSpace = sig 
  type pixel

  val luminance : pixel -> float

  val merge_chans : pixel -> float -> float -> pixel

  val equalized : float -> pixel

end

module type CsConversor = sig
  include ColorSpace
  val pixel_of_rgb : pixel -> pixel
  val rgb_of_pixel : pixel -> pixel
  end

module Rgb = struct
  type pixel = {
    red: float;
    green: float;
    blue: float;
  }
  let luminance p = 0.2126 *. p.red +. 0.7152 *. p.green +. 0.0722 *. p.blue

  let merge_chans p l_in l_out = { red = p.red *. (l_out /. l_in); green = p.green *. (l_out /. l_in); blue = p.blue *. (l_out /. l_in) }

  let equalized v = { red = v; green = v; blue = v }

  let rescale_pixel p base scale = { 
  red = p.red *. scale /. base; 
  green = p.green *. scale /. base; 
  blue = p.blue *. scale /. base
  }

  let rgb_of_values r g b = {red = r; green = g; blue = b}

  let red p = p.red
  let green p = p.green
  let blue p = p.blue

  let pixel_of_rgb p = p
  let rgb_of_pixel p = p

end