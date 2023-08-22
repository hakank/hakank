% https://open.kattis.com/problems/metaprogramming
% 1s
% 2.1 Easy

% Not unexpected: Time Limit Exceeded on 2/11!
% Let's test a proper hash/dict in metaprogramming2.pl. It's accepted.

main :-
    read_string(user_input,_,S),
    split_string(S,"\n","\n",Ss),
    s(Ss,[]).

s([],_).
s([S|Ss],L) :-
    split_string(S," ","",T),
    c(T,L,L1),
    s(Ss,L1).

v(L,Var1,Var2,Val1,Val2) :-
    memberchk(Var1-Val1,L),
    memberchk(Var2-Val2,L).   

c(["define",Val0,Var],L0,L) :-
    number_string(Val,Val0),
    (select(Var-_,L0,L1) ->
        append(L1,[Var-Val],L)
    ;
        append(L0,[Var-Val],L)
    ).
c(["eval",Var1,Op0,Var2],L,L) :-
    (Op0 == "=" ->
        Op = '=='
    ;
        atom_string(Op,Op0)
    ),
    (v(L,Var1,Var2,Val1,Val2) -> 
        (call(Op,Val1,Val2) ->
            T = true
        ;
            T = false
        )
    ;
        T = undefined
    ),
    writeln(T).
        
        
    