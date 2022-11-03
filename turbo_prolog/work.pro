domains
 intlist = integer*
 symlist = symbol*
 charlist = char*
 
predicates

skriva(intlist)
skriva(symlist)
skriva(charlist)

clauses
  skriva([]).
  
  skriva([H|T]) :-
    write(H), nl,
    skriva(T). 
    
    
 
