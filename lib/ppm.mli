type config = {
    ppm_version: int;
    max: float;
    ppm_max: int;
    width: int;
    height: int;
}

type pixel = {
    red: float;
    green: float;
    blue: float;
}

(**
    Reads a ppm header from [in_channel] and returns a tuple that contains:
    - the input channel resulting of reading that header
    - the config read from the header
*)
val read_header : in_channel -> in_channel * config

(**
    Reads from [in_channel] 3 values representing a pixel's colors and advances
    the "file offset" to the next pixel.
    Returns [None] if EOF is reached
*)
val read_pixel : in_channel -> config -> (in_channel * pixel) option