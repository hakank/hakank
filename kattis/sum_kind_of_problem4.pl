% https://open.kattis.com/problems/sumkindofproblem
% 1s
% 1.6 Easy

% Using closed forms for the calculations...
% Using read_string/3.


main :-
    read_string(user_input,10000,S),
    split_string(S,"\n","\n",[_|Ss]),
    s(Ss).
s([]).
s([S|Ss]) :-
    split_string(S," ","",T), maplist(number_string,[I,N],T),   
    A is (N*(N+1))/2,O is N*N,E is N*(N+1),
    format('~d ~d ~d ~d~n', [I,A,O,E]),
    s(Ss).
