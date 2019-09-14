{
exception Lexer_exception of string
}

let integer = ['0'-'9']['0'-'9']*

rule scan = parse
  | [' ' '\t' '\n']+  { scan lexbuf                  }
  | '+'          { Parser.Op(Add)                    }
  | '-'          { Parser.Op(Sub)                    }
  | '*'          { Parser.Op(Mult)                   }
  | '/'          { Parser.Op(Div)                    }
  |"echo"        { Parser.Echo_lex                   }
  |"if"          { Parser.If_lex                     }
  |"while"       { Parser.While_lex                  }
  | integer as s { Parser.Num(int_of_string s)       }
  | eof          { Parser.EOF                        }

{
}
