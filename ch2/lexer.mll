(* header *)
{
	let lineNum = ErrorMsg.lineNum
	let linePos = ErrorMsg.linePos

	let error pos msg = (ErrorMsg.error (pos+1) msg)
    let fail = ErrorMsg.fail

    let lpos lexbuf = Lexing.lexeme_start lexbuf

    let rpos lexbuf = Lexing.lexeme_end lexbuf

    let pos_pair lexbuf = lpos lexbuf, rpos lexbuf

    let nextLine pos = (lineNum := !lineNum + 1; linePos := pos :: !linePos)
}

(* definitions *)
let whitespace = " " | "\t"
let number = ['0'-'9']
let alphanumeric = ['a'-'z' 'A'-'Z' '0'-'9']

(* rules *)
rule tokenize = parse
  | "while" {print_endline (Tokens._WHILE (pos_pair lexbuf)); tokenize lexbuf}
  | "for" {print_endline (Tokens._FOR (pos_pair lexbuf)); tokenize lexbuf}
  | "to" {print_endline (Tokens._TO (pos_pair lexbuf)); tokenize lexbuf}
  | "break" {print_endline (Tokens._BREAK (pos_pair lexbuf)); tokenize lexbuf}
  | "let" {print_endline (Tokens._LET (pos_pair lexbuf)); tokenize lexbuf}
  | "in" {print_endline (Tokens._IN (pos_pair lexbuf)); tokenize lexbuf}
  | "end" {print_endline (Tokens._END (pos_pair lexbuf)); tokenize lexbuf}
  | "function" {print_endline (Tokens._FUNCTION (pos_pair lexbuf)); tokenize lexbuf}
  | "var" {print_endline (Tokens._VAR (pos_pair lexbuf)); tokenize lexbuf}
  | "type" {print_endline (Tokens._TYPE (pos_pair lexbuf)); tokenize lexbuf}
  | "array" {print_endline (Tokens._ARRAY (pos_pair lexbuf)); tokenize lexbuf}
  | "if" {print_endline (Tokens._IF (pos_pair lexbuf)); tokenize lexbuf}
  | "then" {print_endline (Tokens._THEN (pos_pair lexbuf)); tokenize lexbuf}
  | "else" {print_endline (Tokens._ELSE (pos_pair lexbuf)); tokenize lexbuf}
  | "do" {print_endline (Tokens._DO (pos_pair lexbuf)); tokenize lexbuf}
  | "of" {print_endline (Tokens._OF (pos_pair lexbuf)); tokenize lexbuf}
  | "nil" {print_endline (Tokens._NIL (pos_pair lexbuf)); tokenize lexbuf}
  | "," {print_endline (Tokens._COMMA (pos_pair lexbuf)); tokenize lexbuf}
  | ":" {print_endline (Tokens._COLON (pos_pair lexbuf)); tokenize lexbuf}
  | ";" {print_endline (Tokens._SEMICOLON (pos_pair lexbuf)); tokenize lexbuf}
  | "(" {print_endline (Tokens._LPAREN (pos_pair lexbuf)); tokenize lexbuf}
  | ")" {print_endline (Tokens._RPAREN (pos_pair lexbuf)); tokenize lexbuf}
  | "[" {print_endline (Tokens._LBRACK (pos_pair lexbuf)); tokenize lexbuf}
  | "]" {print_endline (Tokens._RBRACK (pos_pair lexbuf)); tokenize lexbuf}
  | "{" {print_endline (Tokens._LBRACE (pos_pair lexbuf)); tokenize lexbuf}
  | "}" {print_endline (Tokens._RBRACE (pos_pair lexbuf)); tokenize lexbuf}
  | "." {print_endline (Tokens._DOT (pos_pair lexbuf)); tokenize lexbuf}
  | "+" {print_endline (Tokens._PLUS (pos_pair lexbuf)); tokenize lexbuf}
  | "-" {print_endline (Tokens._MINUS (pos_pair lexbuf)); tokenize lexbuf}
  | "*" {print_endline (Tokens._TIMES (pos_pair lexbuf)); tokenize lexbuf}
  | "/" {print_endline (Tokens._DIVIDE (pos_pair lexbuf)); tokenize lexbuf}
  | "=" {print_endline (Tokens._EQ (pos_pair lexbuf)); tokenize lexbuf}
  | "<>" {print_endline (Tokens._NEQ (pos_pair lexbuf)); tokenize lexbuf}
  | "<" {print_endline (Tokens._LT (pos_pair lexbuf)); tokenize lexbuf}
  | "<=" {print_endline (Tokens._LE (pos_pair lexbuf)); tokenize lexbuf}
  | ">" {print_endline (Tokens._GT (pos_pair lexbuf)); tokenize lexbuf}
  | ">=" {print_endline (Tokens._GE (pos_pair lexbuf)); tokenize lexbuf}
  | "&" {print_endline (Tokens._AND (pos_pair lexbuf)); tokenize lexbuf}
  | "|" {print_endline (Tokens._OR (pos_pair lexbuf)); tokenize lexbuf}
  | ":=" {print_endline (Tokens._ASSIGN (pos_pair lexbuf)); tokenize lexbuf}
  | ['a'-'z' 'A'-'Z'] (alphanumeric|"_")*  as ident {
      print_endline (Tokens._ID (ident, lpos lexbuf, rpos lexbuf));
      tokenize lexbuf
    }
  | number+ as num_str {
      let num = int_of_string num_str in
      print_endline (Tokens._INT (num, lpos lexbuf, rpos lexbuf));
      tokenize lexbuf
  }
  | '"' {str (Buffer.create 0) lexbuf}
  | "/*" {comment 1 lexbuf}
  | whitespace {tokenize lexbuf}
  | '\r'? '\n' {nextLine (1 + lpos lexbuf); tokenize lexbuf}
  | eof {print_endline (Tokens._EOF (pos_pair lexbuf)); exit 0}
  | _ as s {error (lpos lexbuf) ("Unknown character '" ^ Char.escaped s ^ "'"); fail ();}
and str buf = parse
  | '"' {
      let result = Buffer.contents buf in
      let right = lpos lexbuf in
      let left = right - (String.length result) - 1 in
      print_endline (Tokens._STRING (result, left, right));
      tokenize lexbuf
    }
  | '\\' {escaped buf lexbuf}
  | eof {error (lpos lexbuf) "File ended with unclosed string."; fail ()}
  | '\n' {
      nextLine (1 + lpos lexbuf);
      error (lpos lexbuf) "Strings cannot span multiple lines.";
      ErrorMsg.fail ()
    }
  | _ as s {Buffer.add_char buf s; str buf lexbuf}
and escaped buf = parse
  | "n" {Buffer.add_char buf '\n'; str buf lexbuf}
  | "r" {Buffer.add_char buf '\r'; str buf lexbuf}
  | "t" {Buffer.add_char buf '\t'; str buf lexbuf}
  | number number number as num_str {
      let num = int_of_string num_str in
      if num >= 0 && num <= 255 then Buffer.add_char buf (Char.chr num)
      else (error (lpos lexbuf) ("Invalid ASCII ordinal '" ^ num_str ^ "'"); fail ());
      str buf lexbuf
    }
  | '"' {Buffer.add_char buf '"'; str buf lexbuf}
  | '\\' {Buffer.add_char buf '\\'; str buf lexbuf} 
  | [' ' '\t' '\n']+ '\\' as skip {
      let rec incr_newlines s n = match s with
        "" -> ()
      | _ -> let len = String.length s in
        let hd, tl = (String.get s 0), (String.sub s 1 (len - 1)) in
        (if hd = '\n' then nextLine n; incr_newlines tl (n+1))
      in
      incr_newlines skip (lpos lexbuf);
      str buf lexbuf
    }
and comment level = parse
  | "/*" {comment (level + 1) lexbuf}
  | "*/" {if level = 1 then (tokenize lexbuf) else (comment (level - 1) lexbuf)}
  | '\n' {nextLine (1 + lpos lexbuf); comment level lexbuf}
  | eof {error (lpos lexbuf) "File ended with unclosed comment."; fail ()}
  | _ {comment level lexbuf}

(* trailer *)
