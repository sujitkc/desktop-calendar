%{

%}

%token WS COMMA HYPHEN LPAREN RPAREN COLON EOF NEWLINE
%token <int> INTEGER
%token <string> ID
%token <Calendar.holiday_type> HTYPE

%start parse_calendarday

%type <Calendar.calendarday> parse_calendarday

%%

/* calendarday */
parse_calendarday : holiday { $1 }
  | vacation  { $1 }
;

holiday : date COMMA HTYPE { Calendar.Holiday($1, "Holiday", $3) }

vacation : date HYPHEN date COMMA HTYPE { Calendar.Vacation($1, $3, "Vacation", $5) }

date:   INTEGER  ID INTEGER
    { Calendar.Date($1, (Calendar.month_of_string $2), $3) }
;
%%

