% https://open.kattis.com/problems/aliennumbers
% 1s
% 2.1 Easy

/*
The examples:

9 0123456789 oF8        -> Foo
Foo oF8 0123456789      -> 9
13 0123456789abcdef 01  -> 10011
CODE O!CDE? A?JM!.       -> JAM!

Second example: Foo oF8 0123456789
* alien number: Foo
* alien language: oF0
  This means that we have a base-3 language: [0,1,2]
 
  -> 
  F o o 
  1 0 0 
  -> which is dec(9)
  
* target language: 0123456789
  base 10-language

  We have 9 in dec -> 9

Next example: 13 0123456789abcdef 01
* alien number: 13
* alien lanuage: 0123456789abcdef (base 16)
* target language: 01 (base 2)

   1 3
  -> 1*16+3 = 19 
  -> 19(base 16) = 10011(base 2) = 10011 (indices in string "01")

Last example:CODE O!CDE? A?JM!.       -> JAM!  
* alien number:   CODE
* alien language: O!CDE? (base 6)
* target language: A?JM!. (base 6)

*/

% This was messy, but with to_num/6 and d2b/4 it's now accepted.
% 754 chars
% For some reason (or the usual score reason), this is not in any statistics lists...
main :-
    read_string(user_input,_,S),
    split_string(S,"\n","\n",[_|Ss]),
    s(1,Ss).
s(_,[]).
s(I,[S|Ss]) :-
    split_string(S," ","", X),
    maplist(string_chars,X,[A,L,T]),
    length(A,ALen),    
    length(L,BaseL),
    length(T,BaseT),
    findall(J,(member(V,A),nth0(J,L,V)),As),
    to_num(As,1,ALen,BaseL,0,JsN),
    d2b(JsN,BaseT,[],Res),
    format("Case #~d: ",[I]),
    forall((member(J,Res),nth0(J,T,C)),write(C)),
    nl,
    I1 is I+1,
    s(I1,Ss).

to_num([],_I,_Len,_Base,Num,Num).
to_num([H|T],I,Len,Base,Num0,Num) :-
    Len1 is Len-I,
    Num1 is Num0 + H*(Base^Len1),
    I1 is I+1,
    to_num(T,I1,Len,Base,Num1,Num).

d2b(0,_B,T,T).
d2b(N,B,T0,T) :-
    N > 0,
    R is N mod B,
    N1 is N div B,
    d2b(N1,B,[R|T0],T).
    
/*
% Picat's version
dec_to_base(N, Base) = reverse(Res) =>
  Res = [],
  while (N > 0) 
    R := N mod Base,
    N := N div Base,
    Res := Res ++ [R]
  end.

*/

    