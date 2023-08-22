% https://open.kattis.com/problems/commercials
% 1s
% 1.8 Easy

% Another take. NOPE!

main :-
    read_string(user_input,_,S),
    split_string(S,"\n ","\n ",Ss),
    maplist(number_string,[_,P|Ns],Ss),
    findall(Profit,(p(Ns,X),
                    writeln(x=X),
                    length(X,Len),
                    sum_list(X,XSum),
                    Cost is P*Len,
                    Profit is XSum-Cost
                   )
            ,Ps),
    max_list(Ps,Max),
    writeln(Max).

p(S,B) :-
    append(From,_To,S),
    append(_A,B,From),
    B \= [].