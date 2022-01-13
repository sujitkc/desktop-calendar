exception DesktopCalendarException of string
(*
Returns a list of integers ranging from n1 to n2 - 1
  # let l = range 1 10;;
  val l : int list = [1; 2; 3; 4; 5; 6; 7; 8; 9]
*)
let range n1 n2 =                    
    let rec iter n1 n2 acc =           
      if n1 = n2 then acc              
      else iter (n1 + 1) n2 (n1 :: acc)
    in
    List.rev (iter n1 n2 [])

(*
Returns the first n elements of a list l 
Example:
  # let l = range 1 10;;
  val l : int list = [1; 2; 3; 4; 5; 6; 7; 8; 9]
  # first_n 6 l;;
  - : int list = [1; 2; 3; 4; 5; 6]
*)
let first_n n l =
  let rec iter n l acc =
    match n, l with
      _, []
    | 0, _      -> acc
    | _, h :: t -> (iter (n - 1) t (h :: acc))
  in
  List.rev (iter n l [])

(* Drops the first n elements of the list l and returns the remaining.
  # drop_n 6 l;;
  - : int list = [7; 8; 9]
*)
let rec drop_n n l =
  match n, l with
    _, []     -> []
  | 0, _      -> l
  | _, h :: t -> (drop_n (n - 1) t)

(*
  Splits a list into sublists of 7 elements. The last of these lists
   may have fewer than 7 elements, if it's not a multiple of 7.
  # split_weeks (range 1 31);;
  - : int list list =
  [[1; 2; 3; 4; 5; 6; 7]; [8; 9; 10; 11; 12; 13; 14];
   [15; 16; 17; 18; 19; 20; 21]; [22; 23; 24; 25; 26; 27; 28]; [29; 30]]
*)
let split_weeks rom =
  let rec iter rom acc =
    match rom with
      [] -> acc
    | _ -> iter (drop_n 7 rom) ((first_n 7 rom) :: acc)
  in
  List.rev (iter rom [])

(*
  Returns a list with i occurances of n
# create_repetitive_list 10 5;;
Example:
  - : int list = [10; 10; 10; 10; 10]
*)
let repetitive_list n i =
  let rec iter acc i =
    match i with
      0 -> acc
    | _ -> iter (n :: acc) (i - 1)
  in
  iter [] i

(*
  Returns a list of integer lists.
  Each list of integers represents a week in the given month m of the given year y.
  The First week always starts with Sunday. Which means that, if the month actually
    starts on, say Friday, then the first week will be [0; 0; 0; 0; 0; 1; 2] instead
    of just [1; 2].
  Similarly, the last week always ends on Saturday. This means that, if the month actually
    ends on, say Thursday, then the last week will be [26; 27; 28; 29; 30; 0; 0] instead
    of [26; 27; 28; 29; 30].
Example:
  # month_calendar November 2017;;
  - : int list list =
  [[0; 0; 0; 1; 2; 3; 4]; [5; 6; 7; 8; 9; 10; 11];
   [12; 13; 14; 15; 16; 17; 18]; [19; 20; 21; 22; 23; 24; 25];
   [26; 27; 28; 29; 30; 0; 0]]
*) 
let month_calendar m y =
  let month_list = range 1 ((Calendar.daysInMonth m y) + 1)
  and first_weekday = (Calendar.getWeekDay (Calendar.Date(1, m, y))) in
  let begin_padded_month_list =
    List.append 
      (repetitive_list 0 ((Calendar.week_day_number first_weekday) - 1))
      month_list in
  let padded_month_list =
    List.append 
      begin_padded_month_list
      (repetitive_list
        0 (7 - (List.length begin_padded_month_list) mod 7)) in
  (split_weeks padded_month_list)

(* Reads an input file line by line and returns a list of lines. *)
let read_lines_from_file filename = 
  let chan = open_in filename in
  let rec iter lines =
    try
      let line = input_line chan in (iter (line::lines))
    with End_of_file ->
      close_in chan;
      List.rev lines
  in
  iter []

(* Reads an entired file into a single string. *) 
let read_file filename = 
  let lines = read_lines_from_file filename in
  (List.fold_left (fun a b -> (a ^ "\n" ^ b)) "" lines)

(* Function to parse a single holiday. (Each holiday is placed in a
   single line of the input file.) *)
let parse_holiday str_holiday =
  let lexbuf = Lexing.from_string str_holiday in
  Holidayparser.parse_calendarday Lexer.scan lexbuf

let set_year hl y =
  List.map
    (fun h ->
        match h with
          Calendar.Working(d) -> 
            let Calendar.Date(dd, mm, yy) = d in
            Calendar.Working(Calendar.Date(dd, mm, y))
        | Calendar.Holiday(d, desc, ht) ->
            let Calendar.Date(dd, mm, yy) = d in
            Calendar.Holiday(Calendar.Date(dd, mm, y), desc, ht)
        | Calendar.Vacation(d1, d2, desc, ht) -> 
            let Calendar.Date(dd1, mm1, _) = d1
            and Calendar.Date(dd2, mm2, _) = d2 in
            Calendar.Vacation(Calendar.Date(dd1, mm1, y),
                     Calendar.Date(dd2, mm2, y), desc, ht)
    ) hl

(* Parses the holiday list file and returns a list of holidays. *) 
let holiday_list filename y =
  let get_date day =
    match day with
      Calendar.Working(d) -> d
    | Calendar.Holiday(d, _, _) -> d
    | Calendar.Vacation(d, _, _, _) -> d
  in 
  let str_holidays = read_lines_from_file filename in
  let holidays = List.map parse_holiday str_holidays in
  let h = List.sort (
      fun d1 d2 ->
        let dt1 = get_date d1 and dt2 = get_date d2 in
        if Calendar.is_later dt1 dt2 then 1
        else if Calendar.is_later dt2 dt1 then -1
        else 0
    ) holidays in
  set_year h y

(*rearch code - begin *)
module type DAYNODE =
sig
  type daynodetype =
  | Empty
  | Normal  of int
  | Special of int
  | Weekend of int
  | WeekdayName of Calendar.weekDay

  val day_node :
    day:int ->
    offset:float * float ->
    origin:float * float ->
    tc:bytes -> fc:bytes -> bytes
  val normal_day :
    day:int -> offset:float * float -> origin:float * float -> bytes
  val special_day :
    day:int -> offset:float * float -> origin:float * float -> bytes
  val holi_day : 
    day:int ->
    offset:float * float ->
    origin:float * float -> bytes
  val weekday_name :
    day:Calendar.weekDay -> offset:float * float -> origin:float * float ->
                              bytes
end

module DayNode : DAYNODE =
struct
  type daynodetype =
  | Empty
  | Normal  of int
  | Special of int
  | Weekend of int
  | WeekdayName of Calendar.weekDay

  let decorate ~t ~d = d ^ "{" ^ t ^ "}"
  let bolden = decorate ~d:"\\textbf"
  let colour ~c = decorate ~d:("\\color{" ^ c ^ "}")

  let node ~text ~node_name ~offset ~origin ~tc (* text colour *)
         ~fc (* fill colour *)=
    let offset_x, offset_y = offset and origin_x, origin_y = origin in
    let x = offset_x +. origin_x and y = offset_y +. origin_y in
    let strx = (string_of_float x) and stry = (string_of_float y) in
      "\\node[draw=Black, fill=" ^ fc ^
      ", rounded corners, minimum width = 0.6cm]"
      ^ "(" ^ node_name ^ ")" ^ "at (" ^ strx ^ "," ^ stry ^ ")"
      ^ "{" ^ (colour ~t:text ~c:tc) ^ "};"

  let day_node ~day ~offset ~origin ~tc (* text colour *)
               ~fc (* fill colour *)=
    let strd = string_of_int day in
    node ~text:strd ~node_name:("d" ^ strd) ~offset:offset ~origin:origin
         ~tc:tc ~fc:fc

  let normal_day ~day ~offset ~origin =
    let tc = "Black" and fc = "none" in
    day_node ~day:day ~offset:offset ~origin:origin ~tc:tc ~fc:fc

  let special_day ~day ~offset ~origin =
    let tc = "Blue" and fc = "Brown!20" in
    day_node ~day:day ~offset:offset ~origin:origin ~tc:tc ~fc:fc

  let holi_day ~day ~offset ~origin =
    let tc = "Red" and fc = "none" in
    day_node ~day:day ~offset:offset ~origin:origin ~tc:tc ~fc:fc

  let strweekday wd =
    let map = [
      (Calendar.Sunday,    "Su");
      (Calendar.Monday,    "M");
      (Calendar.Tuesday,   "Tu");
      (Calendar.Wednesday, "W");
      (Calendar.Thursday,  "Th");
      (Calendar.Friday,    "Fr");
      (Calendar.Saturday,  "Sa");
    ] in
    let rec loop = function
      [] -> raise Not_found
    | (wd', strwd) :: t -> if wd' = wd then strwd else loop t
    in
    loop map
    
  let weekday_name ~day ~offset ~origin =
    let tc = "Black" and fc = "Red!20" in
    let strwd = strweekday day in
    node ~text:(bolden strwd) ~node_name:strwd ~offset:offset ~origin:origin
         ~tc:tc ~fc:fc
end

module type MONTHGRID =
sig
  val grid : m:Calendar.month -> y:int ->
             holidays:Calendar.calendarday list ->
             origin:(float * float) -> bytes
end

module MonthGrid (D : DAYNODE) =
struct
  type d = D.daynodetype

  let split_weeks rom =
    let rec iter rom acc =
      match rom with
        [] -> acc
      | _ -> iter (drop_n 7 rom) ((first_n 7 rom) :: acc)
    in
    List.rev (iter rom [])

  (* Based on the holiday/working status of a specific date, 
     suggests a daynodetype to be used in the output. *)
  let get_daynode_type dt holidays =
    let Calendar.Date(day, _, _) = dt in
    if Calendar.is_weekend dt               then D.Weekend(day)
    else if Calendar.is_holiday dt holidays then D.Special(day)
    else                                         D.Normal(day)

  let month_calendar m y holidays =
    let ml = range 1 ((Calendar.daysInMonth m y) + 1) in
    let month_list = List.map 
                      (fun d -> 
                         (get_daynode_type (Calendar.Date(d, m, y)) holidays ))
                      ml in
    let first_weekday = (Calendar.getWeekDay (Calendar.Date(1, m, y))) in
    let begin_padded_month_list =
      List.append 
        (repetitive_list D.Empty
                         ((Calendar.week_day_number first_weekday) - 1))
        month_list in
    let padded_month_list =
      List.append 
        begin_padded_month_list
        (repetitive_list
          D.Empty (7 - (List.length begin_padded_month_list) mod 7)) in
    (split_weeks padded_month_list)

  let strweek wk row ~origin =
    let w = 0.8 and h = 0.6 in
    let yoffset = (float_of_int row) *. -1. *. h in 
    let strday d offset =
      match d with
      | D.Empty             -> ""
      | D.Normal(d')        -> D.normal_day ~day:d' ~offset:(offset, yoffset)
                               ~origin:origin
      | D.Special(d')       -> D.special_day ~day:d' ~offset:(offset, yoffset)
                               ~origin:origin
      | D.Weekend(d')       -> D.holi_day ~day:d' ~offset:(offset, yoffset)
                               ~origin:origin
      | D.WeekdayName(wday) -> D.weekday_name ~day:wday
                               ~offset:(offset, yoffset) ~origin:origin
      in
      let rec loop1 wk col =
        if col = List.length wk then ""
        else (strday (List.nth wk col) (w *. (float_of_int col))) ^ 
             "\n" ^ loop1 wk (col + 1)
      in
      loop1 wk 0

  let grid ~m ~y ~holidays ~origin =
    let mdays = month_calendar m y holidays in
    let wkheader = [
      D.WeekdayName(Calendar.Sunday);
      D.WeekdayName(Calendar.Monday);
      D.WeekdayName(Calendar.Tuesday);
      D.WeekdayName(Calendar.Wednesday);
      D.WeekdayName(Calendar.Thursday);
      D.WeekdayName(Calendar.Friday);
      D.WeekdayName(Calendar.Saturday);
    ] in
    let rec loop2 mtable row =
      if row = List.length mtable then ""
      else (strweek (List.nth mtable row) row origin) ^ "\n" ^ loop2 mtable
                    (row + 1)
    in
    (loop2 (wkheader :: mdays) 0)
end

module MG1 = MonthGrid (DayNode)

module type MONTHNAME =
sig
  val month_name : m:Calendar.month -> origin:float * float -> bytes 
end

module MonthName1 : MONTHNAME =
struct
  let month_name ~m ~origin =
    let xorigin, yorigin = origin in
    let strx = string_of_float xorigin and stry = string_of_float yorigin in
    let pos = "(" ^ strx ^ ", " ^ stry ^ ")" in 
    "\\node[](monthname) at " ^ pos ^ "{\\textsc{\\color{\\monthcolour}" ^
    "\\underline{\\scalebox{1.2}{\\Large " ^ (Calendar.string_of_month m) ^
    "}}}};\n"
end

module type IMAGE =
sig
  val image : imfile:bytes -> origin:float * float -> bytes
end

module Image1 : IMAGE =
struct
  let image ~imfile ~origin =
    let xorigin, yorigin = origin in
    let strx = string_of_float xorigin and stry = string_of_float yorigin in
    let pos = "(" ^ strx ^ ", " ^ stry ^ ")" in 
    "\\node[anchor = center](im) at " ^
    pos ^ "{\\includegraphics[width=0.4\\textwidth]{" ^ imfile ^ "}};"
end

module type SPECIALDAYS =
sig
  val special_days: m:Calendar.month -> holidays:Calendar.calendarday list -> 
    origin:float * float -> bytes 
end

module SpecialDays1 : SPECIALDAYS =
struct
  let month_holidays month holidays =
    List.filter
      (
        fun h ->
          match h with
            Calendar.Working(Calendar.Date(_, m, _)) -> m = month
          | Calendar.Holiday(Calendar.Date(_, m, _), s, ht) -> m = month
          | Calendar.Vacation(
              Calendar.Date(_, m, _),
              Calendar.Date(_, _, _), _, _) -> m = month
      ) holidays

  let holiday_rows month_holidays =
    let rows = List.map
      (
        fun h ->
          match h with
            Calendar.Working(Calendar.Date(d, _, _)) ->
                (string_of_int d) ^ " & working \\\\\n"
          | Calendar.Holiday(Calendar.Date(d, _, _), s, ht) ->
                (string_of_int d) ^ " & " ^ s ^ "\\\\\n"
          | Calendar.Vacation(Calendar.Date(d1, m1, _),
              Calendar.Date(d2, m2, _), s, _) ->
                (string_of_int d1) ^ " " ^ (Calendar.string_of_month m1) ^
                " - " ^ (string_of_int d2) ^ " " ^
                (Calendar.string_of_month m2) ^ " & " ^ s ^ "\\\\\n"
      ) month_holidays in
    List.fold_left (fun x y -> x ^ y) "" rows

  let special_days ~m ~holidays ~origin =
    let xorigin, yorigin = origin in
    let strx = string_of_float xorigin and stry = string_of_float yorigin in
    let pos = "(" ^ strx ^ ", " ^ stry ^ ")" in 
    let month_hlist = month_holidays m holidays in
      "\\node[anchor=south](spdays) at " ^ pos ^ " {\n" ^
      "\\begin{scriptsize}\n" ^
      "\\begin{tabular}{| l @{\\hspace{0.2cm}} p{0.35\\textwidth} |}\n" ^
      "\\hline\n" ^
      holiday_rows month_hlist ^
      "\\hline\n" ^
      "\\end{tabular}" ^
      "\\end{scriptsize}" ^
      "};"
end

module type NOTES =
sig
  val notes : origin:float * float -> bytes
end

module Notes1 : NOTES =
struct
  let notes ~origin =
    let xorigin, yorigin = origin in
    let strx = string_of_float xorigin and stry = string_of_float yorigin in
    let pos = "(" ^ strx ^ ", " ^ stry ^ ")" in
      "\\node[anchor=south, align=left](notes) at " ^ pos ^ " {\n" ^
      "\\begin{tabular}{c}\n" ^
      "\\underline{Notes} \\\\\n" ^
      "\\underline{\\hspace{0.4\\textwidth}} \\\\\n" ^
      "\\underline{\\hspace{0.4\\textwidth}} \\\\\n" ^
      "\\end{tabular}\n" ^ "};"
end

module type VALUES =
sig
  val mname_pos: float * float
  val image_pos: float * float
  val mgrid_pos: float * float
  val notes_pos: float * float
  val spdays_pos: float * float
  val mgrid_sf: float
end

module MonthPageFunctor (D : DAYNODE) (MGrid : MONTHGRID)
        (MonthName : MONTHNAME) (I : IMAGE) (S : SPECIALDAYS) (N : NOTES)
        (V : VALUES) =
struct
  let frame_header = 
    "%% frame - begin %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n" ^
    "\\arrayrulecolor{red}\n" ^
    "\\begin{frame}\n" ^
    "\\begin{center}\n" ^
    "\\resizebox{1.1\\textwidth}{!}{\n" ^
    "\\begin{tikzpicture}\n"
  and frame_footer =
    "\\end{tikzpicture}\n" ^
    "}\n" ^
    "\\end{center}\n" ^
    "\\end{frame}\n" ^
    "%% frame - end %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n"

  let scale text factor = "\\scalebox{" ^ (string_of_float factor) ^ "}{" ^ text ^ "}"

  let month_page ~m ~y ~holidays ~imfile =
    let mtable = MGrid.grid ~m:m ~y:y ~holidays:holidays ~origin:V.mgrid_pos
    in
      frame_header ^ 
      (MonthName.month_name ~m:m ~origin:V.mname_pos) ^
      (I.image ~imfile:imfile ~origin:V.image_pos) ^
      (scale mtable V.mgrid_sf) ^
      (S.special_days ~m:m ~holidays:holidays ~origin:V.spdays_pos) ^
      (N.notes ~origin:V.notes_pos) ^
      frame_footer
end

module Values1 : VALUES =
struct
  let mname_pos = (0., 0.5)
  let image_pos = (5., -2.)
  let mgrid_pos = (-2.5, -1.)
  let notes_pos = (5., -6.5)
  let spdays_pos = (0., -6.5)
  let mgrid_sf = 0.9
end

module Values2 : VALUES =
struct
  let mname_pos = (0., 0.5)
  let image_pos = (5., -2.)
  let mgrid_pos = (-2.5, -1.)
  let notes_pos = (5., -6.5)
  let spdays_pos = (0., -6.5)
  let mgrid_sf = 0.8
end

module MonthPage1 = MonthPageFunctor (DayNode) (MG1) (MonthName1) (Image1)
                     (SpecialDays1) (Notes1) (Values1)

module MonthPage2 = MonthPageFunctor (DayNode) (MG1) (MonthName1) (Image1)
                     (SpecialDays1) (Notes1) (Values2)

module type CALENDAR =
sig
  val generate_calendar: y:int -> title:bytes -> spdaysfile:bytes -> imfiles:bytes list -> string
  val cover_page: y:int -> title:bytes -> imfile:bytes -> bytes
  val last_page: imfile:bytes -> sender:bytes -> bytes
end

module DefaultCalendar : CALENDAR =
struct

  let cover_page ~y ~title ~imfile =
    "\\begin{frame}{}\n" ^
    "\\begin{center}\n" ^
    "\\includegraphics[height=0.6\\textheight]{" ^ imfile ^ "}\n\n" ^
    "\\Large{\\color{Red}" ^ (string_of_int y) ^ "}\n\n" ^
    title ^"\n" ^
    "\\end{center}\n" ^
    "\\end{frame}\n"

  let last_page ~imfile ~sender =
    "\\begin{frame}{}\n" ^
    "\\begin{center}\n" ^
    "\\includegraphics[height=0.6\\textheight]{" ^ imfile ^ "}\n\n" ^
    "With best compliments from " ^ sender ^
    "\\footnote{\\href{https://github.com/sujitkc/desktop-calendar/}" ^
    "{https://github.com/sujitkc/desktop-calendar/}}\n" ^
    "\\end{center}\n" ^
    "\\end{frame}\n"

  let generate_calendar ~y ~title ~spdaysfile ~imfiles =
    let holidays = holiday_list spdaysfile y in
      (cover_page ~y:y ~title:title ~imfile:(List.nth imfiles 0)) ^
      (MonthPage1.month_page ~m:Calendar.January ~y:y ~holidays:holidays
         ~imfile:(List.nth imfiles 1)) ^
      (MonthPage1.month_page ~m:Calendar.February ~y:y ~holidays:holidays
         ~imfile:(List.nth imfiles 2)) ^
      (MonthPage1.month_page ~m:Calendar.March ~y:y ~holidays:holidays
         ~imfile:(List.nth imfiles 3)) ^
      (MonthPage1.month_page ~m:Calendar.April ~y:y ~holidays:holidays
         ~imfile:(List.nth imfiles 4)) ^
      (MonthPage1.month_page ~m:Calendar.May ~y:y ~holidays:holidays
         ~imfile:(List.nth imfiles 5)) ^
      (MonthPage1.month_page ~m:Calendar.June ~y:y ~holidays:holidays
         ~imfile:(List.nth imfiles 6)) ^
      (MonthPage1.month_page ~m:Calendar.July ~y:y ~holidays:holidays
         ~imfile:(List.nth imfiles 7)) ^
      (MonthPage1.month_page ~m:Calendar.August ~y:y ~holidays:holidays
         ~imfile:(List.nth imfiles 8)) ^
      (MonthPage1.month_page ~m:Calendar.September ~y:y ~holidays:holidays
         ~imfile:(List.nth imfiles 9)) ^
      (MonthPage1.month_page ~m:Calendar.October ~y:y ~holidays:holidays
         ~imfile:(List.nth imfiles 10)) ^
      (MonthPage1.month_page ~m:Calendar.November ~y:y ~holidays:holidays
         ~imfile:(List.nth imfiles 11)) ^
      (MonthPage1.month_page ~m:Calendar.December ~y:y ~holidays:holidays
         ~imfile:(List.nth imfiles 12)) ^
      last_page ~imfile:(List.nth imfiles 13) ~sender:"Sujit"
end

module CalendarStyle2 : CALENDAR =
struct
  let cover_page = DefaultCalendar.cover_page
  let last_page = DefaultCalendar.last_page
  let generate_calendar ~y ~title ~spdaysfile ~imfiles =
    let holidays = holiday_list spdaysfile y in
      (cover_page ~y:y ~title:title ~imfile:(List.nth imfiles 0)) ^
      (MonthPage2.month_page ~m:Calendar.January ~y:y ~holidays:holidays
         ~imfile:(List.nth imfiles 1)) ^
      (MonthPage2.month_page ~m:Calendar.February ~y:y ~holidays:holidays
         ~imfile:(List.nth imfiles 2)) ^
      (MonthPage2.month_page ~m:Calendar.March ~y:y ~holidays:holidays
         ~imfile:(List.nth imfiles 3)) ^
      (MonthPage2.month_page ~m:Calendar.April ~y:y ~holidays:holidays
         ~imfile:(List.nth imfiles 4)) ^
      (MonthPage2.month_page ~m:Calendar.May ~y:y ~holidays:holidays
         ~imfile:(List.nth imfiles 5)) ^
      (MonthPage2.month_page ~m:Calendar.June ~y:y ~holidays:holidays
         ~imfile:(List.nth imfiles 6)) ^
      (MonthPage2.month_page ~m:Calendar.July ~y:y ~holidays:holidays
         ~imfile:(List.nth imfiles 7)) ^
      (MonthPage2.month_page ~m:Calendar.August ~y:y ~holidays:holidays
         ~imfile:(List.nth imfiles 8)) ^
      (MonthPage2.month_page ~m:Calendar.September ~y:y ~holidays:holidays
         ~imfile:(List.nth imfiles 9)) ^
      (MonthPage2.month_page ~m:Calendar.October ~y:y ~holidays:holidays
         ~imfile:(List.nth imfiles 10)) ^
      (MonthPage2.month_page ~m:Calendar.November ~y:y ~holidays:holidays
         ~imfile:(List.nth imfiles 11)) ^
      (MonthPage2.month_page ~m:Calendar.December ~y:y ~holidays:holidays
         ~imfile:(List.nth imfiles 12)) ^
      last_page ~imfile:(List.nth imfiles 13) ~sender:"Sujit with Suvarna and Monali"
end

let nn_2022 () =

  let y = 2022
  and home = "/home/sujit/funcoding/desktop-calendar/desktop-calendar/calendars/desktop/2022/nn/" in
  let spdaysfile = home ^ "special-days.txt"
  and imfiles = [
    (home ^ "images/december-nagpur.jpeg");
    (home ^ "images/nachiket-riteshp-shruti.jpeg");
    (home ^ "images/nachiket-suyog-3.jpeg");
    (home ^ "images/shailesh-sujit-cycling.jpeg");
    (home ^ "images/mona-shruti.jpeg");
    (home ^ "images/nachiket-suyog-1.jpeg");
    (home ^ "images/nagpur-diwali.jpeg");
    (home ^ "images/nachiket-riteshp.jpeg");
    (home ^ "images/nachiket-suyog-2.jpeg");
    (home ^ "images/shailesh-sujit-agara.jpeg");
    (home ^ "images/nagpur-diwali.jpeg");
    (home ^ "images/nachiket-riteshp.jpeg");
    (home ^ "images/nachiket-suyog-2.jpeg");
    (home ^ "images/shailesh-sujit-agara.jpeg");
 ] in
 print_string (CalendarStyle2.generate_calendar ~y:y ~title:"Nagpur Nostalgia" ~spdaysfile:spdaysfile ~imfiles:imfiles)


let ruj_2022 () =
  let y = 2022
  and home = "/home/sujit/funcoding/desktop-calendar/desktop-calendar/calendars/desktop/2022/ruj/" in
  let spdaysfile = home ^ "GV-special-days.txt"
  and imfiles = [(home ^ "images/0.jpg");
       (home ^ "images/1.jpg"); (home ^ "images/2.jpg");
       (home ^ "images/3.jpg"); (home ^ "images/4.jpg");
       (home ^ "images/5.jpg"); (home ^ "images/6.jpg");
       (home ^ "images/7.jpg"); (home ^ "images/8.jpg");
       (home ^ "images/9.jpg"); (home ^ "images/10.jpg");
       (home ^ "images/11.jpg"); (home ^ "images/12.jpg");
       (home ^ "images/13.jpg");
  ] in
  print_string (DefaultCalendar.generate_calendar ~y:y ~title:"Kiki's World" ~spdaysfile:spdaysfile ~imfiles:imfiles)

let vermas_2022 () =
  let y = 2022
  and home = "/home/sujit/funcoding/desktop-calendar/desktop-calendar/calendars/desktop/2022/vermas/" in
  let spdaysfile = home ^ "special-days.txt"
  and imfiles = [(home ^ "images/0.jpg");
       (home ^ "images/1.jpg"); (home ^ "images/2.jpg");
       (home ^ "images/3.jpg"); (home ^ "images/4.jpg");
       (home ^ "images/5.jpg"); (home ^ "images/6.jpg");
       (home ^ "images/7.jpg"); (home ^ "images/8.jpg");
       (home ^ "images/9.jpg"); (home ^ "images/10.jpg");
       (home ^ "images/11.jpg"); (home ^ "images/12.jpg");
       (home ^ "images/13.jpg");
  ] in
  print_string (DefaultCalendar.generate_calendar ~y:y ~title:"" ~spdaysfile:spdaysfile ~imfiles:imfiles)

let familypack_2022 () =
  let y = 2022
  and home = "/home/sujit/funcoding/desktop-calendar/desktop-calendar/calendars/desktop/2022/family-pack/" in
  let spdaysfile = home ^ "special-days.txt"
  and imfiles = [(home ^ "images/0.jpg");
       (home ^ "images/1.jpg"); (home ^ "images/2.jpg");
       (home ^ "images/3.jpg"); (home ^ "images/4.jpg");
       (home ^ "images/idukki.jpeg"); (home ^ "images/6.jpg");
       (home ^ "images/7.jpg"); (home ^ "images/8.jpg");
       (home ^ "images/painting.jpeg"); (home ^ "images/10.jpg");
       (home ^ "images/11.jpg"); (home ^ "images/12.jpg");
       (home ^ "images/13.jpg");
  ] in
  print_string (DefaultCalendar.generate_calendar ~y:y ~title:"Family Pack" ~spdaysfile:spdaysfile ~imfiles:imfiles)



(* 
let _ = ruj_2022 ()
let _ = vermas_2022 ()
let _ = familypack_2022 ()
 *)
let _ = nn_2022 ()
(*rearch code - end *)
