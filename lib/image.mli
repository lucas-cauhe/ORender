type pixel = {
    red: float;
    green: float;
    blue: float;
}

val clamp : pixel -> pixel

val equalization : pixel -> float -> pixel

val gamma : pixel -> float -> float -> pixel

val gamma_clamp : pixel -> float -> float -> float -> pixel