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
%type <Source.t> expr

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
	{ Source.Number ($1) }
| VARIABLE
	{ Source.Var ($1) }
| TRUE
	{ Source.Bool (true) }
| FALSE
	{ Source.Bool (false) }
| LPAREN expr RPAREN
    { $2 }
| LRESET expr RRESET
    { Source.Reset ($2) }

expr:
| simple_expr
	{ $1 }
| LAMBDA VARIABLE DOT expr
	{ Source.Lambda ($2, $4) }
| expr APP expr
	{ Source.App ($1, $3) }
