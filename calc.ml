let _ =
  print_string "command # ";flush stdout;
  while true do
    let s = input_line stdin in
    let lexbuf = Lexing.from_string s in
    let result = Parser.buffer Lexer.token lexbuf in
    print_int result; print_string "\ncommand # "; flush stdout
  done
