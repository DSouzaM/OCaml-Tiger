type exp = unit
type level = {parent: level option; frame: Frame.frame}
type access = level * Frame.access
type newLevelParams = {parent: level; name: Temp.label; formals: bool list}

let outermost = 
  let frame = Frame.newFrame{name=Temp.newlabel (); formals=[]} in
  {parent=None; frame=frame}

let newLevel {parent; name; formals} =
  let frame = Frame.newFrame {name=name; formals=true::formals} in
  {parent=Some parent; frame=frame}

let formals level =
  let {frame; _} = level in
  List.map (fun x -> (level, x)) (Frame.formals frame)

let allocLocal ({frame; _} as level) escapes = (level, Frame.allocLocal frame escapes)