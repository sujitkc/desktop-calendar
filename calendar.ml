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

let next_week_day = function
    Sunday    -> Monday
  | Monday    -> Tuesday
  | Tuesday   -> Wednesday
  | Wednesday -> Thursday
  | Thursday  -> Friday
  | Friday    -> Saturday
  | Saturday  -> Sunday

let week_day_number = function
    Sunday    -> 1
  | Monday    -> 2
  | Tuesday   -> 3
  | Wednesday -> 4
  | Thursday  -> 5
  | Friday    -> 6
  | Saturday  -> 7

let prev_week_day = function
    Sunday    -> Saturday
  | Monday    -> Sunday
  | Tuesday   -> Monday
  | Wednesday -> Tuesday
  | Thursday  -> Wednesday
  | Friday    -> Thursday
  | Saturday  -> Friday

let weekday_of_string s =
  match s with
    "Sunday"
  | "SUNDAY"
  | "SUN"       -> Sunday
  | "Monday"
  | "MONDAY"
  | "MON"       -> Monday
  | "Tuesday"
  | "TUESDAY"
  | "TUE"       -> Tuesday
  | "Wednesday"
  | "WEDNESDAY"
  | "WED"       -> Wednesday
  | "Thursday"
  | "THURSDAY"
  | "THU"       -> Thursday
  | "Friday"
  | "FRIDAY"
  | "FRI"       -> Friday
  | "Saturday"
  | "SATURDAY"
  | "SAT"       -> Saturday
  | _           -> raise (CalendarException ("invalid weekday string " ^ s))

let string_of_weekday = function
    Sunday    -> "Sunday"
  | Monday    -> "Monday"
  | Tuesday   -> "Tuesday"
  | Wednesday -> "Wednesday"
  | Thursday  -> "Thursday"
  | Friday    -> "Friday"
  | Saturday  -> "Saturday"

let month_of_string sm =
  match sm with 
    "January"   -> January
  | "February"  -> February
  | "March"     -> March
  | "April"     -> April
  | "May"       -> May
  | "June"      -> June
  | "July"      -> July
  | "August"    -> August
  | "September" -> September
  | "October"   -> October
  | "November"  -> November
  | "December"  -> December
  | _           -> raise (CalendarException ("parseMonth : Invalid month string '" ^ sm ^ "'"))
 
(*
  Given a year y, return true if y is a leap year; false otherwise.
  Example:
    2000 --> true
    2001 --> false
    2012 --> true
    1900 --> false
*)
let isLeapYear y =
  if (y mod 100) = 0 then
    (y mod 400) = 0
  else
    (y mod 4) = 0

let daysInMonth m y =
  match m with
    January   -> 31
  | February  -> if(isLeapYear y) then 29 else 28
  | March     -> 31
  | April     -> 30
  | May       -> 31
  | June      -> 30
  | July      -> 31
  | August    -> 31
  | September -> 30
  | October   -> 31
  | November  -> 30
  | December  -> 31

let nextMonth m =
  match m with
    January   -> February
  | February  -> March
  | March     -> April
  | April     -> May
  | May       -> June
  | June      -> July
  | July      -> August
  | August    -> September
  | September -> October
  | October   -> November
  | November  -> December
  | December  -> January

let string_of_month = function
    January   -> "January"
  | February  -> "February"
  | March     -> "March"
  | April     -> "April"
  | May       -> "May"
  | June      -> "June"
  | July      -> "July"
  | August    -> "August"
  | September -> "September"
  | October   -> "October"
  | November  -> "November"
  | December  -> "December"

let int_of_month = function
    January   -> 1
  | February  -> 2
  | March     -> 3
  | April     -> 4
  | May       -> 5
  | June      -> 6
  | July      -> 7
  | August    -> 8
  | September -> 9
  | October   -> 10
  | November  -> 11
  | December  -> 12


type date = Date of int * month * int

let string_of_date dt =
  let Date(d, m, y) = dt in
  (string_of_month m) ^ " " ^ (string_of_int d) ^ ", " ^ (string_of_int y)

type holiday_type =
    Personal
  | Official

let string_of_holiday_type = function
    Personal -> "Personal"
  | Official -> "Official"

type calendarday =
    Working  of date
  | Holiday  of date * string * holiday_type
  | Vacation of date * date * string * holiday_type

let string_of_calendarday = function
    Working(d) -> (string_of_date d)
  | Holiday(d, m, t)  -> (string_of_date d) ^ ", " ^ m ^ "(" ^ (string_of_holiday_type t) ^ ")"
  | Vacation(d1, d2, m, t) -> (string_of_date d1)
      ^ " - " ^ (string_of_date d2)
      ^ ", " ^ m ^ "(" ^ (string_of_holiday_type t) ^ ")"

(*
  Given a date, return its next date.
  Example:
    (1, January, 2014)   --> (2, January, 2014)
    (31, December, 2013) --> (1, January, 2014)
    (28, February, 2013) --> (1, March, 2013)
    (28, February, 2012) --> (29, February, 2012)
*)
let nextDate dt =
  let Date(d, m, y) = dt in
  if d = (daysInMonth m y) then
    if(m = December) then
      Date(1, January, y + 1)
    else
      Date(1, (nextMonth m), y)
  else
    Date(d + 1, m, y)

(*
  Given two dates dd1 and dd2, return true if dd1 is strictly later than dd2
  Example:
    (1, January, 2014) (1, January, 2014)   --> false
    (1, January, 2014) (2, January, 2014)   --> false
    (1, January, 2014) (31, December, 2013) --> false
*)
let is_later dt1 dt2 =
  let Date(d1, m1, y1) = dt1 and Date(d2, m2, y2) = dt2 in
  if y1 = y2 then
    if m1 = m2 then
      d1 > d2
    else if m1 > m2 then true
    else false
  else if y1 > y2 then true
  else false

let rec daysInBetween d1 d2 =
  let rec iter d1 d2 acc =
    if d1 = d2 then
      acc
    else
      (iter (nextDate d1) d2 (d1::acc))
  in
    if (is_later d1 d2) then
      raise (CalendarException ("Calendar.daysInBetween : d1 " ^ (string_of_date d1) ^ " is later than the d2 " ^ (string_of_date d2)))
    else (iter d1 d2 [])

(*
  Given two dates d1 and d2, count how many days elapse between the two.
  Whether d1 occurs later than d2 shouldn't matter.
  Example:
    (1, January, 2014) (1, January, 2014) --> 0
    (1, January, 2014) (2, January, 2014) --> 1
    (1, January, 2014) (31, December, 2013) --> -1
*)
let rec numOfDaysInBetween d1 d2 =
  let rec iter d1 d2 n =
    if d1 = d2 then
      n
    else
      (iter (nextDate d1) d2 (n + 1))
  in
    if (is_later d1 d2) then
      -(numOfDaysInBetween d2 d1)
    else (iter d1 d2 0)

(*
  Given a weekday wd and a number n, return the weekday of the date
  n days after a wd.
*)
let countWeekDays wd n =
  let rec aux wd n =
      if n = 0 then wd
      else if n > 0 then aux (next_week_day wd) (n - 1)
      else               aux (prev_week_day wd) (n + 1)
  and n' = n mod 7
  in
  aux wd n'

let getWeekDay (d : date) =
  let refDate = Date(21, July, 2014)
  and refWeekDay = Monday
  in
    let delta = numOfDaysInBetween refDate d
    in
      let deltaWeekDay = delta mod 7
      in
        countWeekDays refWeekDay deltaWeekDay

let is_weekend dt =
  let wd = (getWeekDay dt) in
  if (wd = Saturday || wd = Sunday) then true
  else false

(*
  Given two dates d1 and d2, return the list of all dates from d1 to d2, d1 and d2 included.
  Example:
    (1, January, 2014) (5, January, 2014) -->
      [(1, January, 2014); (2, January, 2014); (3, January, 2014);
       (4, January, 2014); (5, January, 2014)]
*)
let rec dateRange d1 d2 =
  let rec iter d1' d2' range =
    if (is_later d1' d2') then (dateRange d2' d1')
    else if (d1' = d2') then (d2' :: range)
    else (iter (nextDate d1') d2' (d1' :: range))
  in
  (List.rev (iter d1 d2 []))
