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

module Rgb : sig
  include CsConversor

  val rescale_pixel : pixel -> float -> float -> pixel

  val rgb_of_values : float -> float -> float -> pixel

  val red : pixel -> float

  val green : pixel -> float
  
  val blue : pixel -> float

end 
