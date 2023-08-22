% https://open.kattis.com/problems/commercials
% 1s
% 1.8 Easy

% Faster version w/o clpfd?
% Nope, still Time Limit Exceeded on 2/11

main :-
    read_string(user_input,1000000,S),
    split_string(S,"\n ","\n ",Ss),
    maplist(number_string,[N,P|Ns],Ss),
    findall(Profit,(between(1,N,I),between(I,N,J),
                    p(I,J,Ns,0,Pp),
                    Cost is P*(J-I+1),
                    Profit is Pp-Cost
                   )
            ,Ps),
    max_list(Ps,Max),
    writeln(Max).

p(I,J,_L,S,S) :- I > J.
p(I,J,L,S0,S) :-
    I =< J,
    nth1(I,L,X),
    S1 is S0+X,
    I1 is I+1,
    p(I1,J,L,S1,S).

