% https://open.kattis.com/problems/oddities
% Time limit: 1s
% Diff 1.3 Easy

% Shorter with read_string/3

main :-
    read_string(user_input,1000,S),
    split_string(S,"\n","\n",[_|Ss]),
    maplist(number_string,Ns,Ss),
    maplist(s,Ns).
s(N) :- (N mod 2 =:= 0 -> T = "even" ; T = "odd" ),
    format('~d is ~s~n',[N,T]).
