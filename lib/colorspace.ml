module type ColorSpace = sig 
  type pixel

  val luminance : pixel -> float

  val merge_chans : pixel -> float -> float -> pixel

  val equalized : float -> pixel

end

type rgb_pixel = {
  red: float;
  green: float;
  blue: float;
}

module Rgb : ColorSpace with type pixel := rgb_pixel = struct
  let luminance p = 0.2126 *. p.red +. 0.7152 *. p.green +. 0.0722 *. p.blue

  let merge_chans p l_in l_out = { red = p.red *. (l_out /. l_in); green = p.green *. (l_out /. l_in); blue = p.blue *. (l_out /. l_in) }

  let equalized v = { red = v; green = v; blue = v }

end