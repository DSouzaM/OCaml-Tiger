(* header *)
{
  open Parser

	let lineNum = ErrorMsg.lineNum
	let linePos = ErrorMsg.linePos

	let error pos msg = (ErrorMsg.error pos ("Lexer error: " ^ msg))

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
  | "while" {WHILE}
  | "for" {FOR}
  | "to" {TO}
  | "break" {BREAK}
  | "let" {LET}
  | "in" {IN}
  | "end" {END}
  | "function" {FUNCTION}
  | "var" {VAR}
  | "type" {TYPE}
  | "array" {ARRAY}
  | "if" {IF}
  | "then" {THEN}
  | "else" {ELSE}
  | "do" {DO}
  | "of" {OF}
  | "nil" {NIL}
  | "," {COMMA}
  | ":" {COLON}
  | ";" {SEMICOLON}
  | "(" {LPAREN}
  | ")" {RPAREN}
  | "[" {LBRACK}
  | "]" {RBRACK}
  | "{" {LBRACE}
  | "}" {RBRACE}
  | "." {DOT}
  | "+" {PLUS}
  | "-" {MINUS}
  | "*" {TIMES}
  | "/" {DIVIDE}
  | "=" {EQ}
  | "<>" {NEQ}
  | "<" {LT}
  | "<=" {LE}
  | ">" {GT}
  | ">=" {GE}
  | "&" {AND}
  | "|" {OR}
  | ":=" {ASSIGN}
  | ['a'-'z' 'A'-'Z'] (alphanumeric|"_")*  as ident {
      ID ident
    }
  | number+ as num_str {
      let num = int_of_string num_str in
      INT num
  }
  | '"' {str (Buffer.create 0) lexbuf}
  | "/*" {comment 1 lexbuf}
  | whitespace {tokenize lexbuf}
  | '\r'? '\n' {nextLine (1 + lpos lexbuf); tokenize lexbuf}
  | eof {EOF}
  | _ as s {error (lpos lexbuf) ("Unknown character '" ^ Char.escaped s ^ "'"); fail ();}
and str buf = parse
  | '"' {
      let result = Buffer.contents buf in
      STRING result
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
        if hd = '\n' then nextLine n;
        incr_newlines tl (n+1)
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
