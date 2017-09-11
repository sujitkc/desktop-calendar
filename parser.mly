%{
%}

%token NEWLINE WS COMMA EOF
%token <int> INTEGER
%token <string> ID

%start input
%type <unit> input

%% /* Grammar rules and actions follow */
input:	 line input 	{ }
  | EOF                 { () }
;

line:	NEWLINE		{ }
	| date NEWLINE	{ Printf.printf "\t%s\n" (Lexer.string_of_date $1); flush stdout }

date:   INTEGER  COMMA ID COMMA INTEGER
    { ($1, (Lexer.month_of_string $3), $5) }
;

%%

