let go () : unit =
  let program : Source.t = Parser.expr Lexer.token (Lexing.from_channel stdin) in
  let source : string = Source.to_string program in
  print_string ("Parsed : \n" ^ source ^ "\n");
  let cps : Target.t = Cps.standard_df_cps program in
  let result : string = Target.to_string cps in
  print_string ("Result : \n" ^ result ^ "\n")

let _ = go ()
