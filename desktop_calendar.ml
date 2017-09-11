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
      (repetitive_list 0 ((Calendar.weekDayNumber first_weekday) - 1))
      month_list in
  let padded_month_list =
    List.append 
      begin_padded_month_list
      (repetitive_list
        0 (7 - (List.length begin_padded_month_list) mod 7)) in
  (split_weeks padded_month_list)

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
 
let read_file filename = 
  let lines = read_lines_from_file filename in
  (List.fold_left (fun a b -> (a ^ "\n" ^ b)) "" lines)

let parse_holiday str_holiday =
  let lexbuf = Lexing.from_string str_holiday in
  Holidayparser.parse_calendarday Lexer.scan lexbuf
 
let holiday_list filename =
  let str_holidays = read_lines_from_file filename in
  List.map parse_holiday str_holidays

(* Based on the holiday/working status of a specific date, suggests a colour to be used in the output. *)
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
let make_month_table (month, string_month) =
  let slide_header = "\\begin{frame}\n\\begin{center}"
  and slide_footer = "\\end{center}\n\\end{frame}" in
  let rec n_header_cells = function
    0 -> ""
  | n -> "c |" ^ (n_header_cells (n - 1)) in
  let theader = "\\begin{center} \n \\begin{tabular}{|" ^ (n_header_cells 7) ^ "}" 
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

  and tfooter = "\n\\hline\n\\end{tabular} \n \\end{center}" in
  let rows = List.map (fun month -> make_row month) string_month in
  let tbody   = List.fold_left (fun a b -> a ^ "\n" ^ b) "" rows in
    slide_header ^ theader ^ tbody ^ "\n" ^ tfooter ^ slide_footer

(* Read the initial common portion of the output latex file from before.tex and return as string *)
let before y =
 let b1 = read_file "b1.tex"
  and b2 = read_file "b2.tex" in
  b1 ^ "\n" ^ "\\date{\\Huge{" ^ (string_of_int y) ^ "}}" ^ "\n" ^ b2

(* Return the last part of the output latex file *)
let after () = "\n" ^ "\\end{document}"

(* Given a list of attendances attendance_lst, generate a latex string corresponding to it. *)
let calendar_to_tex year holidays =
  let tables =
    let month_lst = string_year_calendar year holidays in
    let table_lst = List.map make_month_table month_lst in
    (List.fold_left (fun a b -> a ^ "\n" ^ b) " " table_lst)
  in
  (before year) ^ "\n" ^ tables ^ "\n" ^ (after ())

(* Generate the latex file for the attendance list. *)
let gen_latex_file latex =
  let ochan = open_out ("output/" ^ "desktop_calendar.tex") in
    output_string ochan latex;
    close_out ochan

let main () =
  let num_of_args = Array.length Sys.argv in
  if num_of_args <> 3 then
    print_string "Exactly two command-line argument required.\n"
  else
    let h = (holiday_list Sys.argv.(2)) in
    let latex = (calendar_to_tex (int_of_string Sys.argv.(1)) h) in
    begin
      gen_latex_file latex;
      print_string "generating latex file ...";
      let _ = Sys.command ("pdflatex "
          ^ "-output-directory=output output/"
          ^ "desktop_calendar.tex  >/dev/null") in ();                    (* latex to pdf id *)
      print_string "generated. \n";
      let _ = Sys.command ("rm " ^ "output/" ^ "attendance.tex") in (); (* remove latex file *)
      let _ = Sys.command ("rm " ^ "output/" ^ "attendance.aux") in (); (* remove aux file *)
      let _ = Sys.command ("rm " ^ "output/" ^ "attendance.log") in ()  (* remove log file *)
    end

let _ = main ()

(* 
let gen_pdf (ifile) =
  (ifile |> csv_to_student_list |> (make_lists_of_n rows_per_page) |> attendance_to_tex |> gen_latex_file)();
  print_string "generating latex file ...";
  let _ = Sys.command ("pdflatex "
          ^ "-output-directory=output output/"
          ^ "attendance.tex  >/dev/null") in ();                    (* latex to pdf id *)
  print_string "generated. \n";
(*  let _ = Sys.command ("rm " ^ "output/" ^ "attendance.tex") in (); (* remove latex file *) *)
  let _ = Sys.command ("rm " ^ "output/" ^ "attendance.aux") in (); (* remove aux file *)
  let _ = Sys.command ("rm " ^ "output/" ^ "attendance.log") in ()  (* remove log file *)
*)
