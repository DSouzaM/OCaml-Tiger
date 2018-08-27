module type S =
sig
  type key
  type 'a table
  val empty : 'a table
  val enter : 'a table * key * 'a -> 'a table
  val look : 'a table * key -> 'a option
end

module IntMapTable (X : sig
    type key
    val getInt: key -> int
  end 
) : S with type key=X.key