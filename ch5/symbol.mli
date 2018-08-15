type symbol
val symbol : string -> symbol
val name : symbol -> string
type 'a table
val empty : 'a table
val enter : 'a table * symbol * 'a -> 'a table
val look  : 'a table * symbol -> 'a option