{
    open Parser
    open Lexing
    exception Error of string
}

let space = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let alnum = digit | alpha

rule token = parse
    | '\\'          { L_SLASH                            }
    | '/'           { R_SLASH                                  }
    | ','           { COMMA                                }
    | ':'           { COLON                                }
    | ';'           { SCOLON                                }
    | '?' (alnum+ as lxm) { ITEM (lxm)                  }
    | '$' (alnum+ as lxm) { OPERATOR (lxm)}
    | '%' (digit+ as lxm)  { NUM (int_of_string lxm) }
    | space+        { token lexbuf                         }
    | eof           { EOF                                  }
    | _             { raise (Error (Printf.sprintf
                      "At offset %d: unexpected character.\n"
                      (Lexing.lexeme_start lexbuf))) }
