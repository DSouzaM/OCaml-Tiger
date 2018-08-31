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
) =
struct
  type key = X.key
  module IntMap = Map.Make(
    struct 
      type t=int
      let compare=compare
    end)
  type 'a table = 'a IntMap.t
  let empty = IntMap.empty
  let enter (t, k, a) = IntMap.add (X.getInt k) a t
  let look (t, k) = IntMap.find_opt (X.getInt k) t
end