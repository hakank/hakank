% https://open.kattis.com/problems/conformity
% 1s
% 1.8 Easy

% Perhaps a little convoluted....

main :-
    read_string(user_input,10000000000,S),
    split_string(S,"\n","\n",[_|Ss]),
    s(Ss,[],Ls),
    msort(Ls,Lss),
    clumped(Lss,Cl),
    sort(2,@>=,Cl,[_-Max|_]),
    findall(Max,member(_-Max,Cl),Cs),
    sum_list(Cs,Len),
    writeln(Len).

s([],S,S).
s([L|Ls],S0,[Ts|S]) :-
    split_string(L," ","",T),
    sort(T,Ts),
    s(Ls,S0,S).
        