{
  open A3
  exception Not_implemented
}

(*
  Below is a dummy implementation. Please note the following
  - Tokens are defined in A3.mly
  - Return type is token and not token list
  - End of buffer is indicated by EOF token below
  - There is no trailer. The scanner function is written in the wrapper file (test_a4.ml)
  - This is sample implementation. Please rewrite them as per the specifications
*)

let whitespace = [' ' '\t' '\n' '\r']+
let identifiers = ['A' - 'Z']['A' - 'Z' 'a' - 'z' '_' '\'']*
let integer = ['0'] | ['1'-'9']['0'-'9']*
let no_integer = ['0']['0']+ | ['0']+['1'-'9']['0'-'9']*
let bool_t = 'T'
let bool_f = 'F'
let un_arith = "abs"
let minus = "~"
let un_bool = "not"
let add = '+'
let sub = '-'
let mult = '*'
let div = "div"
let rem = "mod"
let and_ = "/\\"
let or = "\\/"
let equal = '='
let gt = '>'
let lt = '<'
let open_p = '('
let close_p = ')'
let cond_if = "if"
let cond_else = "else"
let cond_then = "then"
let cond_fi = "fi"
let comma = ","
let proj = "proj"
let let_ = "let"
let in_ = "in"
let end_ = "end"
let backslash = "\\"
let def = "def"
let dot = "."
let semicolon = ";"
let parallel = "||"
let local = "local"

rule read = parse
    eof                        { EOF }
  | bool_t                     { BOOL(true) }
  | bool_f                     { BOOL(false) }
  | identifiers as n           { ID(n) }
  | integer as n               { INT(int_of_string n)}
  | no_integer                 { raise Not_implemented }
  | minus                      { TILDA }
  | un_bool                        { NOT }
  | un_arith                   { ABS }
  | add                        { PLUS }
  | sub                        { MINUS }
  | mult                       { TIMES }
  | div                        { DIV }
  | rem                        { REM }
  | and_                       { CONJ }
  | or                         { DISJ }
  | equal                      { EQ }
  | gt                         { GT }
  | lt                         { LT }
  | open_p                     { LP }
  | close_p                    { RP }
  | cond_if                    { IF }
  | cond_else                  { ELSE }
  | cond_then                  { THEN }
  | cond_fi                    { FI }
  | comma                      { COMMA }
  | proj                       { PROJ }
  | let_                       { LET }
  | in_                        { IN }
  | end_                       { END }
  | backslash                  { BACKSLASH }
  | dot                        { DOT }
  | def                        { DEF }
  | semicolon                  { SEMICOLON }
  | parallel                   { PARALLEL }
  | local                      { LOCAL }
  | whitespace                 { read lexbuf }
  | _                          { raise Not_implemented }
