module Dictionary
    type Gaddag

    val empty   : unit -> Gaddag
    val insert  : string -> Gaddag -> Gaddag
    val lookup  : string -> Gaddag -> bool
    val step    : char -> Gaddag -> (bool * Gaddag) option
    val reverse : Gaddag -> (bool * Gaddag) option