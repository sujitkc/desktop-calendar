{
exception Lexer_exception of string
}

let digit = ['0'-'9']
let integer = ['0'-'9']['0'-'9']*
let id = ['a'-'z''A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9']*
rule scan = parse
  | [' ' '\t']+           { scan lexbuf }
  | [' ' '\t']+  { (* print_string "scanned ws"; flush stdout;      *)  scan lexbuf        }
  | '('          { (* print_string "scanned (\n"; flush stdout;     *)  Holidayparser.LPAREN                     }
  | ')'          { (* print_string "scanned )\n"; flush stdout;     *)  Holidayparser.RPAREN                     }
  | '-'          { (* print_string "scanned -\n"; flush stdout;     *)  Holidayparser.HYPHEN                     }
  | integer as s { (* print_string "scanned int\n"; flush stdout;   *)  try Holidayparser.INTEGER((int_of_string s)) with Failure(_) -> print_string "failed here"; exit(0) }
  | id as s      { 
                   (* print_string ("scanned id " ^ s ^ "\n"); flush stdout; *)
                   let s' = String.trim s in
                   if s' = "Personal" then Holidayparser.HTYPE(Calendar.Personal)
                   else if s' = "Official" then  Holidayparser.HTYPE(Calendar.Official)
                   else Holidayparser.ID(s)
                 }
  | ','          {  (* print_string "scanned comma\n"; flush stdout; *) Holidayparser.COMMA                      }
  | '\n'         {                                                      Holidayparser.NEWLINE                    }
  | ';'          { desc (Buffer.create 17) lexbuf }
  | eof          {  (* print_string "scanned eof\n"; flush stdout;  *)  Holidayparser.EOF                        }

and desc buf = parse
  | ';'         {  
                   (* (print_string (Buffer.contents buf)); *)
                   DESC(Buffer.contents buf)
                }
  | [^ ';' ]+   {
                   Buffer.add_string buf (Lexing.lexeme lexbuf);
                   desc buf lexbuf
                }
  | eof         { raise (Failure ("String is not terminated")) }
{
}
