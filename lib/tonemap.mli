module type Image = sig
  type pixel

end

module type PixelConversor = sig
    val pixel_of_rgb : Rgb.t -> Image.pixel
    val rgb_of_pixel : Image.pixel -> Rgb.t
end

module Rgb : Image
module Hsv : Image

module ToneMapper (Pixel: Colorspace.ColorSpace) : Image with type pixel := Pixel.t = sig
    type pixel
    val clamp : ?th:float -> float -> float
    
    val equalization : float -> float -> float
    
    val gamma : float -> float -> float -> float
    
    val gamma_clamp : float -> float -> float -> float -> float

    val tone_map : pixel -> (float -> float) -> pixel
  
end