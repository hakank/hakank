% https://open.kattis.com/problems/pet
% Time limit: 1s
% Difficulty: 1.4 Easy

% A little shorter using read_string/3.

main :-
    read_string(user_input,1000000,S),
    split_string(S,"\n","\n",Ss),
    s(1,Ss,[],Sums),
    sort(2,@>=,Sums,[Winner-Value|_]),
    format('~d ~d~n',[Winner,Value]).    
s(_,[],M,M).
s(I,[S|Ss],M0,[I-Sum|M]) :-
    split_string(S," ","",T),
    maplist(number_string,Ns,T),
    sum_list(Ns,Sum),
    I1 is I+1,
    s(I1,Ss,M0,M).
