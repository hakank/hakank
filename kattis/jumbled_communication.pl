% https://open.kattis.com/problems/communication
% 2s
% 2.1 Easy


main :-
    readln([_|T],end_of_file),
    s(T),
    nl.
s([]).
s([N|T]) :-
    between(0,255,I),
    N =:= (I xor(I<<1)) mod 256,
    format("~d ",[I]),
    s(T).

/*
% Compressed: 123 chars. Top 10 place 5!
main:-readln([_|T],end_of_file),s(T),nl.
s([]). s([N|T]):-between(0,255,I),N=:=(I xor(I<<1))mod 256,format("~d ",[I]),s(T).
*/