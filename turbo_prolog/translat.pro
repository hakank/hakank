include "phrases.pro"

predicates

	find_phrase
	main
	test(string, string)
	
clauses

main if
     nl, write("Write the swedish phrase: "), nl,
     find_phrase.
     

find_phrase   if
     readln(Phrase), 
     phrases(Phrase, Out), test(Phrase,Out).  			
       
test(X,Y) if 
     X = "#", !.
test(X,Y) if 
     Y  = "" , write("I can't find the word."), !, main. 	    	
test(X, Y) if 
     Y<>"", write(Y), main.

     

goal
    write("Welcome to Trans-lator 747!"), nl,
    write("Exit the session with '#'!"),
    main, nl.
              
      
      
      