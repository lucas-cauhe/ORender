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

module Rgb : ColorSpace with type pixel := rgb_pixel