/*             Translate
fr†n AI programming with TP 
sid 96 ff
*/

domains
  sym_list  = symbol*  

predicates
  eval(sym_list)
  parse(symbol, sym_list)
  repeat
  translate
  transform(sym_list, sym_list, symbol)
  write_list(sym_list)
  
clauses
  translate:- 
     write("Propostion:"),
     repeat,
     write("\n\nEnter Propostion or q to quit.\n:  "),
     readln(Statement),
     parse(Statement, State_list),
     eval(State_list).
     
   eval([q]).
   
   eval(State_list):-
     transform(State_list, Final_state, Reason),
     write("\nFinal Propostion = "),
     write_list(Final_state),
     write("\nReduced by - ", Reason),
     eval(Final_State).                         /* g”r en ny eval!  (EGET) */
  
  parse("", []).
  
  parse(Statement, [Token|List]):-
     fronttoken(Statement, Token, Remainder),
     parse(Remainder, List).
/*     
    
  transform(["not", "(", "not", X, ")"],               /* not(not X) */
          [X], "Reduce Double Negation")  :- !.    
        
  transform([X, "=", Y],                               /* X = Y */  
        ["not", "(",  X, "v", Y, ")"],
        "Iliminiate Implication")         :-!.

  transform(["not", "(",  X, "v", Y, ")"],               /* X = Y */  
        [X, "=", Y],
        "Iliminiate Implication 2")         :-!.

  transform(["not", "(", X, "&", Y, ")"],              /* not(X & Y) */
        ["not", "(", X, "v", "not", Y, ")"],
        "De Morgan's Theorem for And")    :- !.
 
 
  transform(["not", "(", X, "v", "not", Y, ")"],              /* not(X & Y) */
        ["not", "(", X, "&", Y, ")"],
        "De Morgan's Theorem for And 2")    :- !.
  
 transform(["not", "(", X, "v", Y, ")"],               /* not(X v Y) */
        ["(", "not", X, ")", "&", "(", "not", Y, ")"],
        "De Morgan's Theorem for Or")     :- !.
 
 transform(["(", "not", X, ")", "&", "(", "not", Y, ")"],     /*  not(X v Y) */
        ["not", "(", X, "v", Y, ")"],
        "De Morgan's Theorem for Or 2")     :- !.
      
  transform([X, "&", Y, "v", Z],                      /* X & Y v Z  */
        ["(", X, "v", Z, ")", "&", 
        "(", Y, "v", Z, ")"], 
        "Distribution")                  :- !.
        
 transform([X, "v", Y, "&", Z],                      /*  X v Y & Z  */
        ["(", X, "v", Z, ")", "&", 
        "(", Y, "v", Z, ")"],
        "Distribution")                  :- !.
*/       
 transform(["(", Test, ")"],                           /* f”rs”k till reduce!*/   
         _, _) :-      
         
         transform(Test, Final_test, Reason_Test).    




 transform(_, [], "") :-
        write("\nInitial proposition was not transformed."), !, fail.
        
 write_list([]).
 write_list([Head|Tail]) :-
        write(" ", Head),
        write_list(Tail).
        
 repeat.
 repeat :- repeat.
 
 
 
        
        
        
        
  
 
  