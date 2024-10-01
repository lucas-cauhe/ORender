module type ToneMapper = sig
    type pixel
    val tone_map : pixel -> (float -> float) -> pixel
    val clamp : ?th:float -> float -> float
    
    val equalization : float -> float -> float
    
    val gamma : float -> float -> float -> float
    
    val gamma_clamp : float -> float -> float -> float

end

module Image (Pixel: Colorspace.ColorSpace) : ToneMapper with type pixel := Pixel.pixel