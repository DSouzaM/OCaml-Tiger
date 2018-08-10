type symbol = string * int

module H = Hashtbl

exception Symbol

let nextsym = ref 0
let hashtable : (string, int) H.t = H.create 1000

let symbol name =
    match H.find_opt hashtable name with
      Some i -> (name, i)
    | None -> let i = !nextsym in 
        nextsym := i+1;
	      H.add hashtable name i;
	      (name, i)

let name (s, n) = s

module Tbl = Table.IntMapTable(struct 
  type key = symbol
  let getInt (s,n) = n
end)

type 'a table = 'a Tbl.table
let empty = Tbl.empty
let enter = Tbl.enter
let look = Tbl.look