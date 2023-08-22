% https://open.kattis.com/problems/statistics
% 1s
% 2.0 Easy

main :-
    read_string(user_input,10000000,S),
    split_string(S,"\n","\n",Ss),
    s(1,Ss).
s(_,[]).
s(I,[S|Ss]) :-
    split_string(S," ","",T),
    maplist(number_string,[_|Ns],T),
    min_list(Ns,Min),
    max_list(Ns,Max),
    R is Max-Min,
    format("Case ~d: ~d ~d ~d~n",[I,Min,Max,R]),
    I1 is I+1,
    s(I1,Ss).