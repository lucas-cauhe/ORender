(*
  Authors: Lucas Cauhé Viñao, Andrei Dumbrava
  Description: Colorspace interface
*)

(**
  Colorspace module defines computations associated with a pixel type and ways to convert between different colorspaces.
  It also declares specific colorspaces such as RGB.
*)

module type ColorSpace = sig 
  (** pixel representation *)
  type pixel

  (** Retrieve luminance value from a specific colorpsace *)
  val luminance : pixel -> float

  (** Merge luminance and chrominance channels into a pixel *)
  val merge_chans : pixel -> float -> float -> pixel

  (** Return a homogeneous pixel in a specif colorspace *)
  val equalized : float -> pixel

  (** Pretty print a pixel *)
  val show : pixel -> string

  (** Test for total equality *)
  val eq : pixel -> pixel -> bool

end

(** Utility module to convert between colorspaces.
  There may be many colorspace-specific modules such as db where writing or reading from files might need a conversion to or from the 
  colorspace to be used *)
module type CsConversor = sig
  include ColorSpace

  val pixel_of_rgb : pixel -> pixel

  val rgb_of_pixel : pixel -> pixel
end

(** Rgb colorspace implementation *)
module Rgb : sig
  include CsConversor

  (** Scales pixel dividing by the [base] and multiplying by its [scale factor] *)
  val rescale_pixel : pixel -> float -> float -> pixel

  val rgb_of_values : float -> float -> float -> pixel

  (*** GETTERS ***)

  val red : pixel -> float

  val green : pixel -> float
  
  val blue : pixel -> float

end 
