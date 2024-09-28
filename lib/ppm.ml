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

let read_header ic = (ic, {ppm_max = 1; ppm_version = 3; max = 1.; width = 20; height = 20})

let read_pixel ic conf = (ic, {red = 1.; green = 1.; blue = 1.})