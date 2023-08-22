% https://open.kattis.com/problems/cetvrta
% CPU Time Limit: 1s
% Difficulty: 1.4 Easy
% 
% w/o kattio.pl

main :-
    read_ints(X1,Y1),
    read_ints(X2,Y2),
    read_ints(X3,Y3),
    member(X,[X1,X2,X3]),
    member(Y,[Y1,Y2,Y3]),
    not(member([X,Y],[[X1,Y1],[X2,Y2],[X3,Y3]])),    
    format('~d ~d~n',[X,Y]).
main.

read_ints(N1,N2) :-
    read_line_to_string(user_input,S),
    split_string(S, " ","", Ss),
    maplist(number_string,[N1,N2],Ss).
