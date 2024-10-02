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

module Rgb : CsConversor