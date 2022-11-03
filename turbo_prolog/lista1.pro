domains
     list = symbol*
     char_list = char*     

predicates
     write_a_list(list)
     length_of(list, integer)
     v„nd_om_list(list, list)      
     v„nd_om_symbol(list, symbol)
     v„nd_om_char(char_list, char)
     main
clauses
     
     write_a_list([H|T]) if
          write(H), nl,
           write_a_list(T). 
    write_a_list([H|_]).
    
    /* Denna v„nder en lista! OBS Allt „r listor!*/
    
    v„nd_om_list([H|T], List2) if 
        v„nd_om_list(T, List2),
        v„nd_om_list(List2, List3). 
    
    v„nd_om_list([H|_], [H]).    
    
    /* Dito, men anv„nder symboler!!!*/
    
    v„nd_om_symbol([H|T], List1) if 
        v„nd_om_symbol(T, List1),
        v„nd_om_symbol([List1], List2). 
    v„nd_om_symbol([H|_], H).    
        
    v„nd_om_char([H|T], List1) if 
        v„nd_om_char(T, List1),
        v„nd_om_char([List1], List2). 
   v„nd_om_char([H|_], H).    
    

  main  if
   write(" Skriv ett ord!\n"),
   readln(Input),
   str_char(Input, X),
   v„nd_om_char(Lista, X).
         
    
    
     length_of([], 0).
     length_of([_|T],L) if
           length_of(T, TailLength),
           L = TailLength + 1.

/*                 
goal
   write_a_list([kalle, olle, nisse]).
   length_of([kalle, olle, nisse], L), write(L).*/
   