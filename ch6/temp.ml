type temp = int

let temps = ref 100
let newtemp () =
  let t = !temps in
  let _ = temps := t+1 in
  t
let makestring temp = "t" ^ (string_of_int temp)

module Table = Table.IntMapTable(struct
  type key = int
  let getInt n = n
end)

type label = Symbol.symbol

let labels = ref 0

let newlabel () =
  let n = !labels in
  let _ = labels := n+1 in
  Symbol.symbol ("L" ^ (string_of_int n))

let namedlabel = Symbol.symbol