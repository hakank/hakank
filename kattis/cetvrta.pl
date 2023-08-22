% https://open.kattis.com/problems/cetvrta
% CPU Time Limit: 1s
% Difficulty: 1.4 Easy
% 
:- [kattio].

main :-
    read_int(X1),read_int(Y1),
    read_int(X2),read_int(Y2),
    read_int(X3),read_int(Y3),
    check([[X1,Y1],[X2,Y2],[X3,Y3]],[X,Y]),
    format('~d ~d~n',[X,Y]).
main.


check([[X1,Y1],[X2,Y2],[X3,Y3]],[X,Y]) :-
    member(X,[X1,X2,X3]),
    member(Y,[Y1,Y2,Y3]),
    not(member([X,Y],[[X1,Y1],[X2,Y2],[X3,Y3]])).