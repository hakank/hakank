

       
predicates
	alfa_token(string, char, string, integer)
     /*   alfa_token1(string, char, string, integer)*/
        main
        /*loop(integer, integer)*/
        
        
clauses
	

/*        loop(_,_).   
	
	loop(Count, Limit) :-
	   bound(Count),
	   Count < Limit,
	   write(Count),
	   Nc = Count + 1,
	   loop(Nc, Limit).*/
	   
        alfa_token("",X,Y,Z).
	alfa_token(Input, Char, RestString, Len) if
             bound(Len),
             Len > 0,
             frontchar(Input, Char, RestString2), 
	     write(Input,"\n"),
	     str_char(Str, Char),
	     concat(RestString2, Str, NewStr),
	     NewLen = Len - 1 ,
	     alfa_token(NewStr, Char1, Reststring3, NewLen).
        
        alfa_token(Input, _, RestString, Len) :-
        fronttoken(Input, Char, RestString),
        str_len(RestString, Len2),
        alfa_token(RestString, Char2, Rest3, Len2).
           
/*        alfa_token1(Input, Char, RestString, Len) if
             frontchar(Input, Char1, RestString2), 
             write(Input, "\n"),
             alfa_token1(RestString2, Char2, Reststring3, Len).
             
        alfa_token1(Input, Char, RestString, Len) :-
        alfa_token(Input, Char, RestString, Len).
             
  */      


        main if
            write("Input, tack\n"),
	    readln(Input), str_len(Input, Len),
	    alfa_token(Input, Char, RestString, Len).
            
            
            
            
            
            