exception CalendarException of string
type month =
    January
  | February
  | March
  | April
  | May
  | June
  | July
  | August
  | September
  | October
  | November
  | December
type weekDay =
    Sunday
  | Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
val next_week_day : weekDay -> weekDay
val week_day_number : weekDay -> int
val prev_week_day : weekDay -> weekDay
val weekday_of_string : string -> weekDay
val string_of_weekday : weekDay -> string
val month_of_string : string -> month
val isLeapYear : int -> bool
val daysInMonth : month -> int -> int
val nextMonth : month -> month
val string_of_month : month -> string
val int_of_month : month -> int

type date = Date of int * month * int
val string_of_date : date -> string
val is_weekend : date -> bool

type holiday_type =
    Personal
  | Official

type calendarday =
    Working  of date
  | Holiday  of date * string * holiday_type
  | Vacation of date * date * string * holiday_type

val string_of_calendarday : calendarday -> string
val nextDate : date -> date
val is_later : date -> date -> bool
val daysInBetween : date -> date -> date list
val numOfDaysInBetween : date -> date -> int
val countWeekDays : weekDay -> int -> weekDay
val getWeekDay : date -> weekDay
val dateRange : date -> date -> date list
