predicates

alfa(string, integer, integer, integer)
main(string)
pickchar(string, char, integer, integer)

clauses

/*********************************    
    alfa(String, Char) :-
    frontchar(String, Char, Rest),
    str_len(String, L),
    v„nd(String, Char, Rest, L, 1),
    alfa2(Rest, Char2).
   
    alfa2(String, Char) :-
    frontchar(String, Char, Rest),
    str_len(String, L),
    v„nd(String, Char, Rest, L, 1).
    alfa2(Rest, Char2).

    


v„nd(String, Char, Rest, L, L_Counter) :-
    str_char(S_char, Char),
    concat(Rest, S_char, NyStr„ng),
    write(NyStr„ng, "\n"),
    L > L_Counter,
    frontchar(Nystr„ng, Char1, Rest1),
    NyCount = L_Counter + 1,
    v„nd(Rest, Char1, Rest1, L, NyCount).
    
v„nd(String, Char, Rest, _,_) :-!.
***********************************************/
main(String) :-
 str_len(String, L),
 alfa(String, Char, 1, 1).


alfa(String, Char, Nr, Count) :-
   pickchar(String, Char, Nr, Count),
   NewNr = Nr + 1,
   alfa(String, Char, NewNr, Count).

pickchar(String, Object, Nr, C) :-
   Nr > C, 
   frontchar(String, Object, Rest),
   C2 = C + 1, 
   pickchar(Rest, Object1, Nr, C2).
pickchar(String, Object, Nr, C) :-
     frontchar(String, Object, _),
     Nr = C,
     write(Object).
  
  
  