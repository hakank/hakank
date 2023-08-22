% https://open.kattis.com/problems/differentdistances
% 1s
% 1.7 Easy

% Ah, it should be
%   abs(...)^P + abs(...)^P 

main :-
    read_string(user_input,_,S),
    split_string(S,"\n","\n",Ss),
    s(Ss).
s([]).
s(["0"]).
s([L|Ls]) :-
    split_string(L," ","",N0),
    maplist(number_string,[X1,Y1,X2,Y2,P],N0),
    (P>0 -> D is (abs(X2-X1)^P + abs(Y2-Y1)^P)^(1/P) ; D is 0 ),
    format('~6f~n',[D]),
    s(Ls).
