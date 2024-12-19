val square : float -> float
(** Raise the given number to the power of two *)

type texture_map = {
    width : int;
    height : int;
    data : Cairo.Image.data32
}