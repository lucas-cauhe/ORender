type config = {
    ppm_version: string;
    max: float;
    ppm_max: int;
    width: int;
    height: int;
}

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
val read_pixel : in_channel -> config -> Image.pixel option

val write_header : out_channel -> config -> out_channel

val write_pixel : out_channel -> config -> Image.pixel -> unit