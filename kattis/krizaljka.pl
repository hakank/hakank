% https://open.kattis.com/problems/krizaljka
% 1s
% 1.8 Easy

main :-
    read_string(user_input,100000,S),
    split_string(S," "," \n",Ss),
    maplist(string_chars,Ss,[A,B]),
    length(A,AL),
    length(B,BL),
    between(1,AL,I),
    between(1,BL,J),
    nth1(I,A,C),
    nth1(J,B,C),
    s(A,B,1,AL,I,J,C).
main.

s(_,[],_S,_Len,_I,_J,_C).
s(A,[B|Bs],S,Len,I,J,C) :-
    (S =:= J ->
        format('~s~n',[A])
    ;
        w(B,1,I,Len)
    ),
    S1 is S+1,
    s(A,Bs,S1,Len,I,J,C).
    
w(_A,I,_,Len) :- I > Len,nl.
w(A,I,J,Len) :-
    (I=:=J ->
        write(A)
    ;
        write(.)
    ),
    I1 is I+1,
    w(A,I1,J,Len).
    