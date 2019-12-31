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
  let background_image =
    "{\n"
    ^ "\\usebackgroundtemplate{\n"
    ^ "\\tikz\\node[opacity=0.3] {\\includegraphics[width=\\paperwidth]{" ^ image ^ "}};\n"
    ^ "}\n"

  and slide_header = "\\begin{frame}\n\\begin{center}\n"
    ^ "\\begin{tabular}{c @{\\hspace{1cm}} c}\n"
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
    ^ "\\begin{tabular}{| l @{\\hspace{0.5cm}} l |}\n"
    ^ "\\hline\n"
    ^ (List.fold_left (fun a b -> a ^ b) "" holiday_rows)
    ^ "\\hline\n"
    ^ "\\end{tabular}\n"
    ^ "\\end{scriptsize}\n"
    ^ "\\end{minipage}\n"
    ^ "&\n"
    ^ "\\fcolorbox{\\phbordercolour}{white}{\\includegraphics[width=0.35\\textwidth]{"^ image ^"}}\n"
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
    background_image ^ slide_header ^ theader ^ tbody ^ "\n" ^ tfooter ^ slide_footer

(* Read the initial common portion of the output latex file from before.tex and return as string *)
let before inp =
  let b1 = read_file "b1.tex"
  and b2 = read_file "b2.tex" in
    b1 ^ "\n" ^ 
    "\\date{\\Huge{" ^ (string_of_int inp.year) ^ "}}" ^ "\n" ^
    "\\newcommand{\\imagedir}{" ^ inp.image_dir ^ "}" ^ "\n" ^
    b2

let last_slide = 
    "{\n"
    ^ "\\usebackgroundtemplate{\n"
    ^ "\\tikz\\node[opacity=0.3] {\\includegraphics[width=\\paperwidth]{" ^ "{\\imagedir}/13.jpg" ^ "}};\n"
    ^ "}\n" ^

"
\\begin{frame}{}

\\begin{center}
\\fcolorbox{\\phbordercolour}{white}{\\includegraphics[height=0.6\\textheight]{{\\imagedir}/13.jpg}}
\\end{center}
\\myheader{With best compliments from Sujit \\footnote{Powered by OCaml and \\LaTeX}}
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

  
let _ = main ()
(* let _ = test_holiday_list () *)
