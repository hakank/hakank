% https://open.kattis.com/problems/gcd
% 1s
% 1.7 Easy

main :-
    readln([A,B]),
    format("~d",[gcd(A,B)]).

/*
% Compressed: 44 chars
main:-readln([A,B]),format("~d",[gcd(A,B)]).
*/

/*
main :-
    read_string(user_input,10000,S),
    split_string(S," ","\n",Ss),
    maplist(number_string,[A,B],Ss),
    G is gcd(A,B),
    writeln(G).
*/