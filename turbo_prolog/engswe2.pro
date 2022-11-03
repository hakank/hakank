/*include "phrases.pro"*/


predicates

	find_phrase(string)
	main
	test(string, string)
        phrases(string, string)
        	
clauses

main if
     nl, write("Write the english phrase: "), nl,
     readln(Phrase),
     find_phrase(Phrase).
    
find_phrase(Phrase) if 
     phrases(Out, Phrase), write(Out).

find_phrase(Phrase)   if
     fronttoken(Phrase, Fras, Rest), !,
     phrases(Out, Fras), write(Out, " "), 
     frontchar(Rest,_,Rest1),
     find_phrase(Rest1).  		

phrases("hejsan", "hello").
phrases("hur m†r du", "how are you").
phrases("jag", "i").
phrases("m†r", "feel").
phrases("bra", "fine").
phrases("hur", "how").

/*     
test(X, Y,_) if 
     Y = "#", !.*/
/*test(X, Y) if 
     X  = "" , write("I can't find the word."), 
     !, main.*/ 	    	
test(X, Y) if 
     X <> "", write(X).

     

goal
    write("Welcome to Trans-lator 747!"), nl,
    write("Exit the session with '#'!"),
    main, nl.
              
      
      
      