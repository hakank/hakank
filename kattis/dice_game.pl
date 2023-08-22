% https://open.kattis.com/problems/dicegame
% 1s
% 1.6 Easy

% As expected this brute force approach does not work:
% Time Limit Exceeded on test 13/38

main :-
    read_string(user_input,100000,S),
    split_string(S,"\n ","\n ",Ss),
    maplist(number_string,[G1A,G1B,G2A,G2B,E1A,E1B,E2A,E2B],Ss),
    p([G1A,G1B,G2A,G2B],Gs,GSum),
    p([E1A,E1B,E2A,E2B],Es,ESum),    
    sum_list(Gs,GSum),
    sum_list(Es,ESum),    
    findall(P, (member(G,Gs),member(E,Es),P is G-E),Ps),
    sum_list(Ps,PSum),
    (PSum =:= 0 ->
        T="Tie"
    ;
        (PSum > 0 ->
            T="Gunnar"
        ;
            T="Emma"
        )
    ),
    writeln(T),
    nl.

p([D1A,D1B,D2A,D2B],Ds,DSum) :-
    findall(D,(between(D1A,D1B,A),between(D2A,D2B,B),D is A+B),Ds),
    sum_list(Ds,DSum).
    
          
