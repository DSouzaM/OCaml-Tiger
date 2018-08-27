type access = InFrame of int
            | InReg of Temp.temp

type frame = {name: Temp.label; formals: access list; locals: int ref}

let buildFormalAccesses formals =
  let combine (n_frame, lst) = function
    true -> (n_frame+1, InFrame((n_frame+1)*4)::lst)
  | false -> (n_frame, InReg(Temp.newtemp ())::lst)
  in
  (* build the list from left to right (so in-frame params
    that appear later have higher memory addresses), but the
    resultant list is reversed *)
  let (_, reversedAccesses) = List.fold_left combine (0, []) formals in  
  List.rev reversedAccesses

type newFrameParams = {name: Temp.label; formals: bool list}
let newFrame ({name; formals} : newFrameParams) =
  {name=name; formals=buildFormalAccesses formals; locals=ref 0}

let name ({name; _}: frame) = name

let formals ({formals; _}: frame) = formals

let allocLocal ({locals; _}: frame) = function
  true ->
    let _ = locals := !locals + 1 in
    InFrame(!locals * -4)
| false -> InReg(Temp.newtemp ())