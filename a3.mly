%{
    open A1
%}

/*
- Tokens (token name and rules) are modified wrt to A2. Please make necessary changes in A3
- LP and RP are left and right parenthesis
- Write grammar rules to recognize
  - >= <= from GT EQ LT tokens
  - if then else fi
*/
/* Tokens are defined below.  */
%token <int> INT
%token <bool> BOOL
%token <string> ID
%token ABS TILDA NOT PLUS MINUS TIMES DIV REM CONJ DISJ EQ GT LT LP RP IF THEN ELSE FI COMMA PROJ
LET IN END BACKSLASH DOT DEF SEMICOLON PARALLEL LOCAL EOF
%start def_parser exp_parser
%type <A1.definition> def_parser /* Returns definitions */
%type <A1.exptree> exp_parser /* Returns expression */
%%
/* The grammars written below are dummy. Please rewrite it as per the specifications. */

/* Implement the grammar rules for expressions, which may use the parser for definitions */


/* Implement the grammar rules for definitions, which may use the parser for expression  */
def_parser:
    define EOF                  { $1 }

define:
    basic_def                          { $1 }
  | define SEMICOLON basic_def         { Sequence([$1;$3])}
  | define PARALLEL basic_def          { Parallel([$1;$3])}


basic_def:
    DEF ID EQ exp                     { Simple($2,$4) }
  | LOCAL define IN define END         { Local($2,$4)}
;

exp_parser:
    exp EOF                     { $1 }

exp:
    or_exp                    { $1 }
  | exp DISJ or_exp           { Disjunction($1,$3) }

or_exp:
    and_exp                   { $1 }
  | or_exp CONJ and_exp       { Conjunction($1,$3) }

and_exp:
    not_exp                    { $1 }
  | NOT and_exp                { Not($2) }

not_exp:
      comp_exp                     { $1 }
    | not_exp EQ comp_exp          { Equals($1,$3) }
    | not_exp GT comp_exp          { GreaterT($1,$3) }
    | not_exp LT comp_exp          { LessT($1,$3) }
    | not_exp GT EQ comp_exp          { GreaterTE($1,$4) }
    | not_exp LT EQ comp_exp          { LessTE($1,$4) }

comp_exp:
      as_exp                  { $1 }
    | comp_exp PLUS as_exp    { Add($1,$3) }
    | comp_exp MINUS as_exp   { Sub($1,$3) }

as_exp:
      div_exp                   { $1 }
    | as_exp TIMES div_exp     { Mult($1,$3)}
    | as_exp DIV div_exp       { Div($1,$3) }
    | as_exp REM div_exp       { Rem($1,$3) }

div_exp:
      abs_exp                   { $1 }
    | ABS div_exp               { Abs($2) }
    | TILDA div_exp             { Negative($2) }

abs_exp:
      ifte                      { $1 }
    | IF exp THEN exp ELSE exp FI  { IfThenElse($2,$4,$6) }

ifte:
      proj                      { $1 }
    | PROJ LP INT COMMA INT RP ifte          { Project(($3,$5),$7)}

proj:
      tuple_exp                  { $1 }
    | LP exp COMMA exp tail                { Tuple(2+List.length($5),($2 :: ($4::$5))) }

tail:
      COMMA exp tail                { $2 :: $3 }
    | RP                           { [] }

tuple_exp:
      paren                       { $1 }
    | LP exp RP                  { InParen($2) }
    | LET define IN exp END      { Let($2,$4) }

paren:
      cons                        { $1 }
    | Fabs                        { $1 }
    | Fabs LP exp RP              { FunctionCall($1,$3) }
    | LP exp RP LP exp RP                  { FunctionCall($2,$5) }
    | ID LP exp RP                       { FunctionCall(Var($1),$3) }

Fabs:
      BACKSLASH ID DOT cons        { FunctionAbstraction($2,$4) }
    | BACKSLASH ID DOT LP exp RP        { FunctionAbstraction($2,$5) }

cons:
      BOOL                        { B($1) }
    | INT                         { N($1) }
    | ID                          { Var($1) }
;
