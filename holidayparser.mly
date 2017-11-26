%{

%}

%token WS COMMA HYPHEN LPAREN RPAREN SEMICOLON EOF NEWLINE
%token <int> INTEGER
%token <string> ID
%token <string> DESC
%token <Calendar.holiday_type> HTYPE

%start parse_calendarday

%type <Calendar.calendarday> parse_calendarday

%%

/* calendarday */
parse_calendarday : holiday { $1 }
  | vacation  { $1 }
;

holiday : date DESC HTYPE { Calendar.Holiday($1, $2, $3) }

vacation : date HYPHEN date DESC HTYPE { Calendar.Vacation($1, $3, $4, $5) }

date:   INTEGER  ID
    { Calendar.Date($1, (Calendar.month_of_string $2), 0) }
;
%%

