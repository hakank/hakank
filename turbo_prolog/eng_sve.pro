include "phrases.pro"

predicates

	find_phrase
	main
	test(string, string)
	
clauses

main if
     nl, write("Write the english phrase: "), nl,
     find_phrase.
     

find_phrase   if
     readln(Phrase), 
     phrases(Out, Phrase), test(Out, Phrase).  			
       
test(X,Y) if 
     Y = "#", !.
test(X,Y) if 
     X  = "" , write("I can't find the word."), !, main. 	    	
test(X, Y) if 
     X<>"", write(X), main.

     

goal
    write("Welcome to Trans-lator 747!"), nl,
    write("Exit the session with '#'!"),
    main, nl.
              
      
      
      