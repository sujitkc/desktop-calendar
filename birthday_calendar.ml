exception Birthday_Calendar_exception of string

let bday_messages = [
  "Happy birthday!";
  "Wish you a very happy birthday!";
  "Have a great birthday and a fabulous year!";
  "Many happy returns of the day!";
  "Many many happy returns of the day!"
]

let month_num  = function
  | Calendar.January   -> 1
  | Calendar.February  -> 2
  | Calendar.March     -> 3
  | Calendar.April     -> 4
  | Calendar.May       -> 5
  | Calendar.June      -> 6
  | Calendar.July      -> 7
  | Calendar.August    -> 8
  | Calendar.September -> 9
  | Calendar.October   -> 10
  | Calendar.November  -> 11
  | Calendar.December  -> 12

let month_of_int n =
  match n with
  |  1 -> Calendar.January
  |  2 -> Calendar.February
  |  3 -> Calendar.March
  |  4 -> Calendar.April
  |  5 -> Calendar.May
  |  6 -> Calendar.June
  |  7 -> Calendar.July
  |  8 -> Calendar.August
  |  9 -> Calendar.September
  | 10 -> Calendar.October
  | 11 -> Calendar.November
  | 12 -> Calendar.December
  |  _ -> failwith ("Invalid month number " ^ (string_of_int n))

module Record = struct
  type record = {
    roll_num : string;
    name     : string;
    birthday : Calendar.date
  }

  let compare_birthdays bday1 bday2 =
    let Calendar.Date(d1, m1, _) = bday1
    and Calendar.Date(d2, m2, _) = bday2 in
   let n1 = month_num m1
    and n2 = month_num m2 in
    if n1 < n2 then -1
    else if n1 > n2 then 1
    else
      if d1 < d2 then -1
      else if d1 > d2 then 1
      else 0

  let sort_records records =
    List.sort (
      fun r1 r2 ->
        let bday1 = r1.birthday
        and bday2 = r2.birthday in
        compare_birthdays bday1 bday2
    ) records

  let string_of_record r =
    "{ " ^ r.roll_num ^ "; " ^ r.name ^ "; " ^ (Calendar.string_of_date r.birthday) ^ " }"
end

let csv_to_record_list file_name =
  let allrows = Csv.load file_name in
  match allrows with
    [] ->  raise (Birthday_Calendar_exception "empty input")
  | _  ->
    List.map
    (
      fun row ->
        match row with
          [ rn; name;  strdate ] ->
            let dlist = Str.split (Str.regexp "/") strdate in
            begin
              match dlist with
                [ strd; strm; stry ] ->
                  {
                    Record.roll_num = rn;
                    Record.name = name;
                    Record.birthday = Calendar.Date(
                      (int_of_string strd),
                      (month_of_int (int_of_string strm)),
                      (int_of_string stry)
                    )
                  }
              | _ -> failwith ("Couldn't understand date " ^ strdate)
            end
        | _ -> raise (Birthday_Calendar_exception "Invalid CSV row")
    )
    allrows

let pick_from_list l =
  let index = Random.int (List.length l) in
  List.nth l index

let birthday_frame flowers r =
  let Calendar.Date(d, m, _) = r.Record.birthday in
  let bday = (Calendar.string_of_month m) ^ " " ^ (string_of_int d) in
  let slide_header = "\\begin{frame}{Happy Birthday " ^ r.Record.name ^ "}\n{" ^
    bday ^ "}\n\\begin{center}\n"
  and msg = (pick_from_list bday_messages) ^ r.Record.name
  and flower = (pick_from_list flowers) in
  let flower_image = "\\includegraphics[height=0.5\\textheight]{flowers/" ^ flower ^ "}\n\n"
  and slide_footer = "\n\\end{center}\n\\end{frame}\n"
  in
  slide_header ^ flower_image ^ msg ^ slide_footer

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

(* Generate the latex file for the attendance list. *)
let gen_latex_file latex =
  let ochan = open_out ("output/" ^ "birthday_calendar.tex") in
    output_string ochan latex;
    close_out ochan

let gen_birthday_calendar () =
  let flowers = Array.to_list (Sys.readdir "output/flowers")
  and records = csv_to_record_list "input/2018/birthdays.csv" in
  let sorted_records = (Record.sort_records records) in
  let frames = List.map (birthday_frame flowers) sorted_records in
  let all_wishes = List.fold_left (fun x y -> x ^ y) "" frames in
  let before = read_file "before.tex" in
  let after = "\n" ^ "\\end{document}" in
  let latex = before ^ all_wishes ^ after in
  gen_latex_file latex


let t1 () =
  let r1 = {
    Record.roll_num = "imt2018001";
    Record.name     = "student 1";
    Record.birthday = Calendar.Date(1, Calendar.January, 1990)
  }
  and r2 = {
    Record.roll_num = "imt2018002";
    Record.name     = "student 2";
    Record.birthday = Calendar.Date(31, Calendar.March, 1989)
  }
  and r3 = {
    Record.roll_num = "imt2018003";
    Record.name     = "student 3";
    Record.birthday = Calendar.Date(1, Calendar.December, 1990)
  }
  and r4 = {
    Record.roll_num = "imt2018004";
    Record.name     = "student 4";
    Record.birthday = Calendar.Date(28, Calendar.February, 1989)
  }
  in
  let records = [ r1; r2; r3; r4 ] in
  List.iter (
    fun r -> print_endline (Record.string_of_record r)
  ) (Record.sort_records records)

let t2 () =
  let records = csv_to_record_list "input/2018/imt2014.csv" in
  List.iter (
    fun r -> print_endline (Record.string_of_record r)
  ) (Record.sort_records records)

let t3 () =
  let dir = "flowers" in
  let children = Sys.readdir dir in
  Array.iter print_endline children;;

let main () =
  gen_birthday_calendar ()

let _ = main ()
