%{
  open Syntax
  (* ここに書いたものは，ExampleParser.mliに入らないので注意 *)
%}

%token <int>    INT
%token <bool>   BOOL 
%token <string> ID
%token LET IN				  
%token PLUS TIMES MINUS DIV
%token OR AND XOR NEQ
%token EQ LT
%token IF THEN ELSE
%token COMMA SEMI
%token LPAR RPAR
%token FUN ARROW
%token REC LAND
%token LBRACKET RBRACKET CONS COMMA
%token MATCH WITH BAR UNDER
%token SEMISEMI

%start toplevel 
%type <Syntax.command> toplevel
%% 

toplevel:
  | expr SEMISEMI                        { CExp($1) }
  | LET var EQ expr SEMISEMI             { CDecl($2, $4) }
  | LET REC let_and_decls SEMISEMI       { CRecDecl($3) }
  | LET var vars EQ expr SEMISEMI        { CFunDecl($2,$3,$5) }
;

vars:
  | var vars                             { $1 :: $2 }
  | var                                  { [$1] }
;

tuple:
  | expr COMMA tuple                     { $1 :: $3 }
  | expr COMMA expr                      { [$1;$3] }
;

list: 
  | expr SEMI list                       { $1 :: $3 }
  | expr                                 { [$1] }
;

let_and_decls:
  | var vars EQ expr LAND let_and_decls  { ($1,$2,$4) :: $6 }
  | var EQ FUN vars ARROW expr 
    LAND let_and_decls                   { ($1,$4,$6) :: $8 }
  | var EQ LPAR FUN vars ARROW expr 
    RPAR LAND let_and_decls              { ($1,$5,$7) :: $10 }
  | var vars EQ expr                     { [($1,$2,$4)] }
  | var EQ FUN vars ARROW expr           { [($1,$4,$6)]}
  | var EQ LPAR FUN vars ARROW expr RPAR { [($1,$5,$7)]}
;

expr:
  | LET var EQ expr IN expr              { ELet($2,$4,$6) }
  | LET REC let_and_decls IN expr        { ELetRec($3,$5) }
  | IF expr THEN expr ELSE expr          { EIf($2,$4,$6) }
  | FUN vars ARROW expr                  { EFun($2,$4) }
  | LET var vars EQ expr IN expr         { ELetFun($2,$3,$5,$7) }
  | MATCH expr WITH cases                { EMatch($2,$4) }
  | MATCH expr WITH BAR cases            { EMatch($2,$5) }
  | atomic_expr CONS expr                { EAppend($1,$3) }
  | bool_expr                            { $1 }
;

cases:
  | pattern ARROW expr                   { [($1,$3)] }
  | pattern ARROW expr BAR cases         { ($1,$3) :: $5 }
;

tuple_pattern:
  | pattern COMMA tuple_pattern          { $1 :: $3 }
  | pattern COMMA pattern                { [$1;$3] }
;

pattern:
  | atomic_pattern CONS pattern          { PCons($1,$3) }
  | atomic_pattern                       { $1 }
;

atomic_pattern:
  | INT                                  { PInt($1) }
  | BOOL                                 { PBool($1) }
  | var                                  { PVar($1) }
  | LPAR tuple_pattern RPAR              { PTuple($2) }
  | LBRACKET RBRACKET                    { PNil }
  | LPAR pattern RPAR                    { $2 }
  | UNDER                                { PWild }
;

bool_expr:
  | bool_expr OR bool_sub_expr           { EOr($1,$3) }  
  | bool_expr AND bool_sub_expr          { EAnd($1,$3) } 
  | bool_expr XOR bool_sub_expr          { EXor($1,$3) } 
  | bool_expr NEQ bool_sub_expr          { ENeq($1,$3) }
  | bool_sub_expr                        { $1 }
;

bool_sub_expr:
  | arith_to_bool_expr                   { $1 }
  | arith_expr                           { $1 }
;

arith_to_bool_expr:
  | arith_expr EQ arith_expr             { EEq($1,$3) }
  | arith_expr LT arith_expr             { ELt($1,$3) }
;

arith_expr:
  | arith_expr PLUS factor_expr          { EAdd($1,$3) }
  | arith_expr MINUS factor_expr         { ESub($1,$3) }
  | factor_expr                          { $1 }
;

factor_expr:
  | factor_expr TIMES app_expr           { EMul($1,$3) }
  | factor_expr DIV app_expr             { EDiv($1,$3) }
  | app_expr                             { $1 }
;

app_expr:
  | app_expr atomic_expr                 { EApp($1,$2) }
  | atomic_expr                          { $1 }
;

atomic_expr:
  | INT                                  { EConstInt($1) }
  | BOOL                                 { EConstBool($1) }
  | var                                  { EVar($1) }
  | LPAR expr RPAR                       { $2 }
  | LPAR tuple RPAR                      { ETuple($2) }
  | LBRACKET list RBRACKET               { EList($2) }
  | LPAR RPAR                            { ETuple([]) }
  | LBRACKET RBRACKET                    { EList([]) }
;
 
var:
  | ID                                   { $1 }
; 
