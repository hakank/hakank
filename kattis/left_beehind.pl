% https://open.kattis.com/problems/leftbeehind
% 1s
% 1.7 Easy

% x = sweet  y = sour
% If Bill has more jars of sour honey than sweet, he will be left “beehind”.
%   -> "Left beehind."
% If Bill has more jars of sweet honey than sour he will go to the convention.
%   -> "To the convention".
% If Bill has the same number of sweet and sour jars, his friends are undecided.
%   -> "Undecided."
% Lastly, Bill’s friends are superstitious, if he has exactly 13 jars they will
% never speak to him again. Bill needs new friends.
%   -> "Never speak again."

main :-
    read_string(user_input,10000,S),
    split_string(S,"\n","\n",Ss),
    s(Ss).

s([]).
s(["0 0"]).
s([S|Ss]) :-
    split_string(S," ","",L),
    maplist(number_string,[X,Y],L),
    T is X + Y,
    t(X,Y,T,M),
    writeln(M),
    s(Ss).
t(_X,_Y,13,"Never speak again.") :- !.
t(X,X,_,"Undecided.") :- !.
t(X,Y,_,"Left beehind.") :- Y > X, !.
t(X,Y,_,"To the convention.") :- X > Y, !.


