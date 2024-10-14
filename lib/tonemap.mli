(*
  Authors: Lucas Cauhé Viñao, Andrei Dumbrava
  Description: IO interface
*)

(** Tonemapping utilities module *)

(**
    Tonemapping main functions implementation
    [CS] is a functor of any type implementing a [ColorSpace] and it is required for computing luminance and other 
    colorspace-related needs
*)
module ToneMapper (CS: Colorspace.ColorSpace) : sig
    (** Clamp value with a given threshold [th] *)
    val clamp : ?th:float -> float -> float
    
    (** Equalize value *)
    val equalization : float -> float -> float
    
    (** Gamma function implementation 
       Receives max_value [k], gamma value [g], input luminance [l_in] 
    *)
    val gamma : float -> float -> float -> float
    
    (** Gamma + clamp implementation 
        Same parameter order as above but with clamp threshold [th] before input luminance
    *)
    val gamma_clamp : float -> float -> float -> float -> float

    (** Main tone map function.
        Receives a [CS.pixel] to tonemap and a tonemapping function only to receive the luminance parameter
        Returns a pixel resulting of the prior tonemapped
    *)
    val tone_map : CS.pixel -> (float -> float) -> CS.pixel
end
