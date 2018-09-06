{
exception Lexer_exception of string
}

let integer = ['0'-'9']['0'-'9']*
let id = ['a'-'z''A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9']*

rule scan = parse
  | [' ' '\t']+  { scan lexbuf                              }
  | '('          { Holidayparser.LPAREN                     }
  | ')'          { Holidayparser.RPAREN                     }
  | '-'          { Holidayparser.HYPHEN                     }
  | integer as s { try
                     Holidayparser.INTEGER((int_of_string s))
                   with Failure(_) ->
                     print_string "Couldn't convert string to integer."; exit(0)
                 }
  | id as s      { 
                   (* print_string ("scanned id " ^ s ^ "\n"); flush stdout; *)
                   if s = "Personal" then Holidayparser.HTYPE(Calendar.Personal)
                   else if s = "Official" then  Holidayparser.HTYPE(Calendar.Official)
                   else Holidayparser.ID(s)
                 }
  | ','          { Holidayparser.COMMA                      }
  | '\n'         { Holidayparser.NEWLINE                    }
  | ';'          { desc (Buffer.create 17) lexbuf           }
  | eof          { Holidayparser.EOF                        }

and desc buf = parse
  | ';'         {  
                   Holidayparser.DESC(Buffer.contents buf)
                }
  | [^ ';' ]+   {
                   Buffer.add_string buf (Lexing.lexeme lexbuf);
                   desc buf lexbuf
                }
  | eof         { raise (Failure ("String is not terminated")) }
{
}
