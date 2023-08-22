% https://open.kattis.com/problems/oddmanout
% 1s
% 1.6 Easy

% Shorter with read_string/3.

main :-
    read_string(user_input,10000000,S),
    split_string(S,"\n","\n",[_|Ss]),
    c(1,Ss).
c(_,[]).
c(I,[_,L|Ls]) :-
    split_string(L," ","",Ss),
    msort(Ss,Sort),
    clumped(Sort,Cl),
    member(W-1,Cl),
    format('Case #~d: ~s~n',[I,W]),
    I1 is I+1,
    c(I1,Ls).
