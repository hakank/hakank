% https://open.kattis.com/problems/oddmanout
% 1s
% 1.6 Easy

main :-
    rs(_),
    once(ra(L)),
    c(1,L).
c(_,[]).
c(I,[_,L|Ls]) :-
    split_string(L," ","",Ss),
    msort(Ss,Sort),
    clumped(Sort,Cl),
    member(W-1,Cl),
    format('Case #~d: ~s~n',[I,W]),
    I1 is I+1,
    c(I1,Ls).

ra(S):-rs(In),ra(In,[],S).
ra(end_of_file,S,S).
ra(In,S0,[In|S]):-rs(S2),ra(S2,S0,S).
rs(S):-read_line_to_string(user_input,S).
