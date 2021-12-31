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

type input = {
  year : int;
  image_dir : string;
}

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

(* Parses the holiday list file and returns a list of holidays. *) 
let holiday_list filename =
  let get_date day =
    match day with
      Calendar.Working(d) -> d
    | Calendar.Holiday(d, _, _) -> d
    | Calendar.Vacation(d, _, _, _) -> d
  in 
  let str_holidays = read_lines_from_file filename in
  let holidays = List.map parse_holiday str_holidays in
  List.sort (
      fun d1 d2 ->
        let dt1 = get_date d1 and dt2 = get_date d2 in
        if Calendar.is_later dt1 dt2 then 1
        else if Calendar.is_later dt2 dt1 then -1
        else 0
    ) holidays

(* Based on the holiday/working status of a specific date, 
   suggests a colour to be used in the output. *)
let strdate_colour day holidays =
  let is_holiday () =
    let rec iter hl =
      match hl with
        [] -> false
      | h :: t ->
      (
        match h with
          Calendar.Working(d) -> if d = day then false else iter t
        | Calendar.Holiday(d, _, _) -> 
            if d = day then true else iter t
        | Calendar.Vacation(d1, d2, _, _) -> if List.mem day (d2 :: (Calendar.daysInBetween d1 d2)) then true else iter t
      )
    in iter holidays
  in
  if Calendar.is_weekend day then "\\weekendcolour"
  else if is_holiday () then "\\holidaycolour"
  else "\\workingdaycolour"

(*
  Returns a list of string lists. Each string list is a week of the month with
  days represented as strings.
Example:
  # string_month_calendar November 2017;;
  - : string list list =
  [["  "; "  "; "  "; " 1"; " 2"; " 3"; " 4"];
   [" 5"; " 6"; " 7"; " 8"; " 9"; "10"; "11"];
   ["12"; "13"; "14"; "15"; "16"; "17"; "18"];
   ["19"; "20"; "21"; "22"; "23"; "24"; "25"];
   ["26"; "27"; "28"; "29"; "30"; "  "; "  "]]
*)
let string_month_calendar m y holidays =
  let string_week w =
    List.map (
      fun d ->
        if d = 0 then "  "
        else "  {\\color{" ^ (strdate_colour (Calendar.Date(d, m, y)) holidays) ^ "} " ^ (string_of_int d) ^ "}"
    ) w
  and mc = month_calendar m y in
  List.map string_week mc

(*
  Given a year y, returns a list of string months
*)
let string_year_calendar y holidays =
  let allmonths = Calendar.[
    January; February; March;     April;   May;      June;
    July;    August;   September; October; November; December ] in
  List.map (fun m -> m, (string_month_calendar m y holidays)) allmonths
























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
    day:Calendar.weekDay -> offset:float * float -> origin:float * float -> bytes
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
      "\\node[draw=Black, fill=" ^ fc ^ ", rounded corners, minimum width = 0.6cm]"
      ^ "(" ^ node_name ^ ")" ^ "at (" ^ strx ^ "," ^ stry ^ ")"
      ^ "{" ^ (colour ~t:text ~c:tc) ^ "};"

  let day_node ~day ~offset ~origin ~tc (* text colour *) ~fc (* fill colour *)=
    let strd = string_of_int day in
    node ~text:strd ~node_name:("d" ^ strd) ~offset:offset ~origin:origin ~tc:tc
         ~fc:fc

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
    node ~text:(bolden strwd) ~node_name:strwd ~offset:offset ~origin:origin ~tc:tc ~fc:fc
end

module type MONTHGRID =
sig
  val grid : m:Calendar.month -> y:int -> holidays:Calendar.calendarday list -> origin:(float * float) -> bytes
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
        (repetitive_list D.Empty ((Calendar.week_day_number first_weekday) - 1))
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
      | D.WeekdayName(wday) -> D.weekday_name ~day:wday  ~offset:(offset, yoffset)
                               ~origin:origin
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
      else (strweek (List.nth mtable row) row origin) ^ "\n" ^ loop2 mtable (row + 1)
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
          | Calendar.Vacation(Calendar.Date(_, m, _), Calendar.Date(_, _, _), _, _) -> m = month
      ) holidays

  let holiday_rows month_holidays =
    let rows = List.map
      (
        fun h ->
          match h with
            Calendar.Working(Calendar.Date(d, _, _)) -> (string_of_int d) ^ " & working \\\\\n"
          | Calendar.Holiday(Calendar.Date(d, _, _), s, ht) -> (string_of_int d) ^ " & " ^ s ^ "\\\\\n"
          | Calendar.Vacation(Calendar.Date(d1, m1, _), Calendar.Date(d2, m2, _), s, _) ->
              (string_of_int d1) ^ " " ^ (Calendar.string_of_month m1) ^ " - " ^
              (string_of_int d2) ^ " " ^ (Calendar.string_of_month m2) ^ " & " ^ s ^ "\\\\\n" 
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

module MonthPage (D : DAYNODE) (MGrid : MONTHGRID) (MonthName : MONTHNAME) 
                  (I : IMAGE) (S : SPECIALDAYS) (N : NOTES) =
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
    let mtable = MGrid.grid ~m:m ~y:y ~holidays:holidays ~origin:(-2.5, -1.)
    in
      frame_header ^ 
      (MonthName.month_name ~m:m ~origin:(0., 0.5)) ^
      (I.image ~imfile:imfile ~origin:(5., -2.)) ^
      (scale mtable 0.9) ^
      (S.special_days ~m:m ~holidays:holidays ~origin:(0., -6.5)) ^
      (N.notes ~origin:(5., -6.5)) ^
      frame_footer
end

module MonthPage1 = MonthPage (DayNode) (MG1) (MonthName1) (Image1) (SpecialDays1) (Notes1)

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

let cover_page ~y ~title ~imfile =
  "\\begin{frame}{}\n" ^
  "\\begin{center}\n" ^
  "\\includegraphics[height=0.6\\textheight]{" ^ imfile ^ "}\n\n" ^
  "\\Large{\\color{Red}" ^ (string_of_int y) ^ "}\n\n" ^
  title ^"\n" ^
  "\\end{center}\n" ^
  "\\end{frame}\n"


let last_page ~imfile =
  "\\begin{frame}{}\n" ^
  "\\begin{center}\n" ^
  "\\includegraphics[height=0.6\\textheight]{" ^ imfile ^ "}\n\n" ^
  "With best compliments from Sujit" ^
  "\\footnote{\\href{github.com/sujitkc/desktop-calendar/}" ^
  "{github.com/sujitkc/desktop-calendar/}}\n" ^
  "\\end{center}\n" ^
  "\\end{frame}\n"

let generate_calendar ~y ~title ~spdaysfile ~imfiles =
  let hl = holiday_list spdaysfile in
  let holidays = set_year hl y in
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
    last_page ~imfile:(List.nth imfiles 13)

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
  print_string (generate_calendar ~y:y ~title:"Kiki's World" ~spdaysfile:spdaysfile ~imfiles:imfiles)


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
  print_string (generate_calendar ~y:y ~title:"" ~spdaysfile:spdaysfile ~imfiles:imfiles)


(* 
let _ = ruj_2022 ()
 *)
let _ = vermas_2022 ()
(*rearch code - end *)



























(* Transforms a string representation of a week into its LaTeX representation. *)
let make_row strweek =
  let rec iter w =
    match w with
      d :: [] -> d ^ " \\\\"
    | d :: t  -> d ^ " & " ^ (iter t)
    | _ -> raise (DesktopCalendarException "Bad week string")
  in
  iter strweek

(* Given a string month, generate a latex table for it. *)
let make_month_table (month, string_month) holidays =
  let image = "{\\imagedir}/"
    ^ (string_of_int (Calendar.int_of_month month)) ^ ".jpg" in
  (*
  let background_image =
    "{\n"
    ^ "\\usebackgroundtemplate{\n"
    ^ "\\tikz\\node[opacity=0.3] {\\includegraphics[width=\\paperwidth]{" ^ image ^ "}};\n"
    ^ "}\n"
  *)
  let slide_header = "\\begin{frame}\n\\begin{center}\n"
    ^ "\\begin{tabular}{l @{\\hspace{.2cm}} r}\n"
    ^ "\\begin{minipage}{0.6\\textwidth}\n"
    ^ "\\vspace{-4cm}\n" in

  let month_holidays = List.filter
    (
      fun h ->
        match h with
          Calendar.Working(Calendar.Date(_, m, _)) -> m = month
        | Calendar.Holiday(Calendar.Date(_, m, _), s, ht) -> m = month
        | Calendar.Vacation(Calendar.Date(_, m, _), Calendar.Date(_, _, _), _, _) -> m = month
    ) holidays in
  let holiday_rows = List.map
    (
      fun h ->
        match h with
          Calendar.Working(Calendar.Date(d, _, _)) -> (string_of_int d) ^ " & working \\\\\n"
        | Calendar.Holiday(Calendar.Date(d, _, _), s, ht) -> (string_of_int d) ^ " & " ^ s ^ "\\\\\n"
        | Calendar.Vacation(Calendar.Date(d1, m1, _), Calendar.Date(d2, m2, _), s, _) ->
            (string_of_int d1) ^ " " ^ (Calendar.string_of_month m1) ^ " - " ^
            (string_of_int d2) ^ " " ^ (Calendar.string_of_month m2) ^ " & " ^ s ^ "\\\\\n" 
    ) month_holidays in

  let slide_footer = 
      "\\vspace{1cm}\n"
    ^ "\\begin{scriptsize}\n"
    ^ "\\begin{tabular}{| l @{\\hspace{0.5cm}} p{0.8\\textwidth} |}\n"
    ^ "\\hline\n"
    ^ (List.fold_left (fun a b -> a ^ b) "" holiday_rows)
    ^ "\\hline\n"
    ^ "\\end{tabular}\n"
    ^ "\\end{scriptsize}\n"
    ^ "\\end{minipage}\n"
    ^ "&\n"
    ^ "\\fcolorbox{\\phbordercolour}{white}{\\includegraphics[width=0.4\\textwidth]{"^ image ^"}}\n"
    ^ "\\end{tabular}\n"
    ^ "\\end{center}\n\\end{frame}\n"
  in

  let rec n_header_cells = function
    0 -> ""
  | n -> "c |" ^ (n_header_cells (n - 1)) in
  let theader = "\\begin{tabular}{|" ^ (n_header_cells 7) ^ "}" 
    ^ "\n\\hline"
    ^ "\\multicolumn{7}{| c |}"
    ^ "{\\textsc{\\color{\\monthcolour}\\underline{\\scalebox{1.2}{\\Large "
    ^ (Calendar.string_of_month month)
    ^"}}}} \\\\"
    ^ "\\hline"
    ^ "\\cellcolor{\\headercolour}\\textbf{\\color{mymaroon}Su} & "
    ^ "\\cellcolor{\\headercolour}\\textbf{\\color{mymaroon}Mo} & "
    ^ "\\cellcolor{\\headercolour}\\textbf{\\color{mymaroon}Tu} & "
    ^ "\\cellcolor{\\headercolour}\\textbf{\\color{mymaroon}We} & "
    ^ "\\cellcolor{\\headercolour}\\textbf{\\color{mymaroon}Th} & "
    ^ "\\cellcolor{\\headercolour}\\textbf{\\color{mymaroon}Fr} & "
    ^ "\\cellcolor{\\headercolour}\\textbf{\\color{mymaroon}Sa} \\\\"

  and tfooter = "\n\\hline\n\\end{tabular} \n" in
  let rows = List.map (fun month -> make_row month) string_month in
  let tbody   = List.fold_left (fun a b -> a ^ "\n" ^ b) "" rows in
    (* background_image ^ *)
    slide_header ^ theader ^ tbody ^ "\n" ^ tfooter ^ slide_footer

(* Read the initial common portion of the output latex file from before.tex and return as string *)
let before inp =
  let b1 = read_file "b1.tex"
  and b2 = read_file "b2.tex" in
    b1 ^ "\n" ^ 
    "\\date{\\Huge{" ^ (string_of_int inp.year) ^ "}}" ^ "\n" ^
    "\\newcommand{\\imagedir}{" ^ inp.image_dir ^ "}" ^ "\n" ^
    b2

let last_slide = 
(*
    "{\n"
    ^ "\\usebackgroundtemplate{\n"
    ^ "\\tikz\\node[opacity=0.3] {\\includegraphics[width=\\paperwidth]{" ^ "{\\imagedir}/13.jpg" ^ "}};\n"
    ^ "}\n" ^
*)
"
\\begin{frame}{}

\\begin{center}
\\fcolorbox{\\phbordercolour}{white}{\\includegraphics[height=0.6\\textheight]{{\\imagedir}/13.jpg}}
\\end{center}
%\\myheader{With best compliments from Sujit}
\\end{frame}
"

(* Return the last part of the output latex file *)
let after = last_slide ^ "\n" ^ "\\end{document}"

(* Given a list of attendances attendance_lst, generate a latex string corresponding to it. *)
let calendar_to_tex inp holidays =
  let tables =
    let month_lst = string_year_calendar inp.year holidays in
    let table_lst = List.map (fun m -> (make_month_table m holidays)) month_lst in
    (List.fold_left (fun a b -> a ^ "\n" ^ b) " " table_lst)
  in
  (before inp) ^ "\n" ^ tables ^ "\n" ^ after

(* Generate the latex file for the attendance list. *)
let gen_latex_file latex =
  let ochan = open_out ("output/" ^ "desktop_calendar.tex") in
    output_string ochan latex;
    close_out ochan

let main () =
  let num_of_args = Array.length Sys.argv in
  if num_of_args <> 4 then
    print_endline ("Exactly three command-line argument required.\n" ^
      "Usage: ./desktop_calendar <year> <holiday-file> <image-directory>")
  else
    let y = (int_of_string Sys.argv.(1))
    and hfile = Sys.argv.(2) in

    let h = List.map
      (
        fun hd ->
          match hd with
            Calendar.Working(Calendar.Date(d, m, _))            ->
                Calendar.Working(Calendar.Date(d, m, y))
          | Calendar.Holiday(Calendar.Date(d, m, _), s, ht)     ->
                Calendar.Holiday(Calendar.Date(d, m, y), s, ht)
          | Calendar.Vacation(Calendar.Date(d1, m1, _), 
              Calendar.Date(d2, m2, _), s, ht)                  ->
                Calendar.Vacation(Calendar.Date(d1, m1, y), Calendar.Date(d2, m2, y), s, ht)
      )
      (holiday_list hfile) in

    let image_dir = Sys.argv.(3) in
    let inp = { year = y; image_dir = image_dir; } in
    let latex = (calendar_to_tex inp h) in
    begin
      gen_latex_file latex;
      print_string "generating latex file ...";
      let _ = Sys.command ("pdflatex "
          ^ "-output-directory=output output/"
          ^ "desktop_calendar.tex  >/dev/null") in ();                    (* latex to pdf id *)
      print_string "generated. \n";
(*      let _ = Sys.command ("rm " ^ "output/" ^ "desktop_calendar.tex") in (); (* remove latex file *) *)
      let _ = Sys.command ("rm " ^ "output/" ^ "desktop_calendar.aux") in (); (* remove aux file *)
      let _ = Sys.command ("rm " ^ "output/" ^ "desktop_calendar.log") in ()  (* remove log file *)
    end

let test_holiday_list () =
    let y = 2019 in
    let _ = List.map
      (
        fun hd ->
          match hd with
            Calendar.Working(Calendar.Date(d, m, _))            ->
                Calendar.Working(Calendar.Date(d, m, y))
          | Calendar.Holiday(Calendar.Date(d, m, _), s, ht)     ->
                Calendar.Holiday(Calendar.Date(d, m, y), s, ht)
          | Calendar.Vacation(Calendar.Date(d1, m1, _), 
              Calendar.Date(d2, m2, _), s, ht)                  ->
                Calendar.Vacation(Calendar.Date(d1, m1, y), Calendar.Date(d2, m2, y), s, ht)
      )
      (holiday_list "input/holidays-iiitb-2019.txt") in
    print_endline "done"

  
(* let _ = main () *)
(* let _ = test_holiday_list () *)
