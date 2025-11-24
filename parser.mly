/* This file is distributed under the MIT license.
   See file LICENSE for more details.
*/

%{ open Ast %}
%token <Z.t> CST "constant"
%token <string> ID
%token ADD "+"
%token STAR "*"
%token MIN "-"
%token DIV "/"
%token EQ "="
%token LT "<"
%token GT ">"
%token NE "<>"
%token LE "<="
%token GE ">="
%token TRUE "true"
%token FALSE "false"
%token AND "&&"
%token OR "||"
%token NOT "!"
%token SKIP "skip"
%token SET ":="
%token SEMICOL ";"
%token IF "if"
%token THEN "then"
%token ELSE "else"
%token FI "fi"
%token WHILE "while"
%token DO "do"
%token DONE "done"
%token LPAR "("
%token RPAR ")"
%token EOF

%left STAR DIV
%left ADD MIN
%nonassoc NOT
%left AND OR
%left SEMICOL

%start <Ast.instruction> program

%%
variable:
  | x = ID { x }

arith_expr:
  | z = CST { Cons z }
  | x = variable { Var x }
  | e1 = arith_expr ADD e2 = arith_expr { Add (e1,e2) }
  | e1 = arith_expr STAR e2 = arith_expr { Mul (e1,e2) }
  | e1 = arith_expr DIV e2 = arith_expr { Div (e1,e2) }
  | MIN e = arith_expr { Opp e }
  | e = delimited(LPAR,arith_expr,RPAR) { e }

%inline cmp_op:
  | EQ { Eq }
  | NE { Ne }
  | GT { Gt }
  | LT { Lt }
  | GE { Ge }
  | LE { Le }

boolean_expr:
  | TRUE { BCons true }
  | FALSE { BCons false }
  | e1 = arith_expr op = cmp_op e2 = arith_expr { Comp (op, e1, e2) }
  | e1 = boolean_expr AND e2 = boolean_expr { And (e1, e2) }
  | e1 = boolean_expr OR e2 = boolean_expr { Or (e1, e2) }
  | NOT e = boolean_expr { Not e }
  | e = delimited(LPAR,boolean_expr,RPAR) { e }

instruction:
  | SKIP { Skip }
  | x = variable SET e = arith_expr { Set(x,e) }
  | i1 = instruction SEMICOL i2 = instruction { Seq(i1,i2) }
  | IF c = boolean_expr THEN t = instruction ELSE e = instruction FI
      { If(c,t,e) }
  | WHILE c = boolean_expr DO b = instruction DONE { Loop(c,b) }

program: i = instruction EOF { i }
