let anyErrors = ref false
let fileName = ref ""
let lineNum = ref 1
let linePos = ref [0]

let reset () = (
  anyErrors := false;
  fileName := "";
  lineNum := 1;
  linePos := [0]
)

exception Error

let error pos (msg:string) =
  let rec look (lst, n) = match lst with
    a::rest -> 
      if a <= pos then print_string (":" ^ (string_of_int n) ^ "." ^
                                    (string_of_int (pos-a+1)))
      else look (rest,n-1)
  | _ -> print_string ":0.0"
  in 
    anyErrors := true;
    print_string (!fileName);
    look (!linePos, !lineNum);
    print_string ":";
    print_endline msg

let fail () = raise Error

let checkErrors () = 
  if !anyErrors then fail ()

let impossible msg =
  print_endline ("Error: Compiler bug: " ^ msg ^ "\n");
  raise Error
