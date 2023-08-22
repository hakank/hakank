% https://open.kattis.com/problems/metaprogramming
% 1s
% 2.1 Easy

% Testing dicts instead. And it's accepted! 

main :-
    read_string(user_input,_,S),
    split_string(S,"\n","\n",Ss),
    s(Ss,d{}).

s([],_).
s([S|Ss],D) :-
    split_string(S," ","",T),
    maplist(atom_string,As,T),
    c(As,D,D1),
    s(Ss,D1).

c([define,Val0,Var],D0,D) :-
    atom_number(Val0,Val),
    D = D0.put(Var,Val).

c([eval,Var1,Op0,Var2],D,D) :-
    (Op0 == '=' ->
        Op = '=='
    ;
        Op = Op0
    ),
    ((Val1=D.get(Var1),Val2=D.get(Var2)) ->        
        (call(Op,Val1,Val2) ->
            T = true
        ;
            T = false
        )
    ;
        T = undefined
    ),
    writeln(T).


/*
% Compressed: 391 chars (not short enough for Top 10 shortest: 97..308 chars
main:-read_string(user_input,_,S),split_string(S,"\n","\n",Ss),s(Ss,d{}).
s([],_). s([S|Ss],D):-split_string(S," ","",T),maplist(atom_string,As,T),c(As,D,D1),s(Ss,D1).
c([define,Val0,Var],D0,D):-atom_number(Val0,Val),D=D0.put(Var,Val).
c([eval,Var1,Op0,Var2],D,D):-(Op0=='='->Op='==';Op=Op0),((Val1=D.get(Var1),Val2=D.get(Var2))->(call(Op,Val1,Val2)->T=true;T=false);T=undefined),writeln(T).

*/