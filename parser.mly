%{
%}

%token LPAREN RPAREN
%token LRESET RRESET
%token LAMBDA DOT SHIFT
%token APP
%token <int> NUMBER
%token TRUE FALSE
%token <string> VARIABLE
%token EOF

/* type of nonterminating expression */
%type <Syntax.t> expr

/* declaring a start state */
%start expr

/* ... operators bind less tightly */
%left APP
%nonassoc SHIFT
%nonassoc LAMBDA DOT
/* ... operators bind more tightly */
/* UNARY expressions need parentheses */

/* must include %% for no real reason */
%%

simple_expr:
| NUMBER
	{ Syntax.Number ($1) }
| VARIABLE
	{ Syntax.Variable ($1) }
| TRUE
	{ Syntax.Bool (true) }
| FALSE
	{ Syntax.Bool (false) }
| LPAREN expr RPAREN
    { $2 }
| LRESET expr RRESET
    { Syntax.Reset ($2) }

expr:
| simple_expr
	{ $1 }
| LAMBDA VARIABLE DOT expr
	{ Syntax.lambda ($2, $4) }
| expr APP expr
	{ Syntax.Op ($1, Syntax.App, $3) }
