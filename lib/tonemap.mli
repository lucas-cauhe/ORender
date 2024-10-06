module ToneMapper (CS: Colorspace.ColorSpace) : sig
    val clamp : ?th:float -> float -> float
    
    val equalization : float -> float -> float
    
    val gamma : float -> float -> float -> float
    
    val gamma_clamp : float -> float -> float -> float -> float

    val tone_map : CS.pixel -> (float -> float) -> CS.pixel
end
