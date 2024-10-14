(*
  Authors: Lucas Cauhé Viñao, Andrei Dumbrava
  Description: IO interface
*)

module type Db = sig
  type config
  type pixel
  
  (**
    Reads a ppm header from [in_channel] and returns a tuple that contains:
    - the input channel resulting of reading that header
    - the config read from the header
*)
  val read_header : in_channel -> config

  (**
    Reads from [in_channel] 3 values representing a pixel's colors and advances
    the "file offset" to the next pixel.
    Returns [None] if EOF is reached
  *)
  val read_pixel : in_channel -> config -> pixel option

  val write_header : out_channel -> config -> unit

  val write_pixel : out_channel -> config -> pixel -> unit

end

(** PPM file format interactive IO implementation for Db *)
module Ppm : sig
  include Db with type pixel := Colorspace.Rgb.pixel
  val config_of_values : string -> float -> int -> int -> int -> config
  val config_max : config -> float
  val config_ppm_max : config -> int
  val config_width : config -> int
  val config_height : config -> int
end
