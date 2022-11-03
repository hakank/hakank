/*  The String List Tool 
Denna modul kallas f”r STRLIST.PRO
bl a i TRANSFOR och TRANSF2


*/

domains
   sym_list = symbol*
   
predicates
  strrev(string, string)
  buildlist(string, sym_list)
  reverse(sym_list, sym_list)
  append(sym_list, sym_list, sym_list)
  
clauses
  
  strrev(Srcstr, Trgtstr) :-
     buildlist(Srcstr, List1),
     reverse(List1, List2),
     buildlist(Trgstr, List1).
     
  buildlist("", []) :- !.
  
  buildlist(Str, [First|Tail]) :-
     bound(Str),
     frontstr(1, Str, First, Rest),
     buildlist(Rest, Tail).
     
  buildlist(Str, [First|Tail]) :-
     free(Str),
     bound(First),
     buildlist(Rest, Tail),
     concat(First, Rest, Str).
     
  reverse([],[]).
  reverse([A|B], C):-
     reverse(B, D), append(D, [A], C).
     
  append([A|B], C, [A|D]) :- append(B, C, D).
  append([], X, X).
             
     
     
    
  
     