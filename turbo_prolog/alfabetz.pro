

       
predicates
	alfa_token(string, char, string)
        s„tt_bakom(string, char, string, integer)
        main
        
clauses
	
	
	alfa_token("",_,_).
	alfa_token(Input, Char, RestString) if
             write(Input, "\n"),
	     frontchar(Input, Char, RestString2), 
             s„tt_bakom(RestString2, Char, Lista, 0), 
            /* write(Lista, ",\n"),*/
	     alfa_token(RestString2, Char1, Reststring3). 


s„tt_bakom(Input, Char, Lista, Counter) if
        str_len(Input, L„ngd),
        str_char(Char_str, Char), 
        concat(Input, Char_str, Lista),
        write(Lista, "\n"),
        str_char(Char_str, Char),
        frontchar(Lista, Char1, Lista2),
/*OBS!*/s„tt_bakom(Lista, Char1, Lista2, 0),
        str_char(X, Char1),
        s„tt_bakom(X, Lista1, Lista2, 0),
        str_char(X, Char1),
        write(Lista2, "\n"),                              
        NyCount = Counter+1, NyCount < L„ngd, !,
        s„tt_bakom(Lista2, Char1, Lista3, NyCount).
        
s„tt_bakom(Input, Char, Lista, Counter) if !.                

        main if
            write("Input, tack\n"),
	    readln(Input), alfa_token(Input, Char, RestString).
            
            
            
            
            
            