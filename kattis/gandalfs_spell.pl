% https://open.kattis.com/problems/gandalfsspell
% 1s
% 2.0 Easy


% Using read_string/3 instead of readln/2.
% Yes, that was the problem!
main :-
    read_string(user_input,_,S),
    split_string(S,"\n","\n",[W,T]),
    string_chars(W,Cs),
    split_string(T," ","",Ts),
    maplist(atom_string,Ws,Ts),
    maplist(atom_string,C,Cs),    
    (s(C,Ws,a{},b{}) ->
        R=true
    ;
        R=false
    ),
    writeln(R).
s([],[],A,B) :- dict_size(A,SA),dict_size(B,SB),SA=:=SB.
s([C|Cs],[W|Ws],A0,B0) :-
    ( (X=A0.get(C),Y=B0.get(W)) ->
        ( (X == W,Y == C) -> 
            A1=A0,
            B1=B0
        ;
            fail
        )
    ;
        A1=A0.put(C,W),
        B1=B0.put(W,C)        
    ),
    s(Cs,Ws,A1,B1).



/*
% Using pairs_keys_values/3: Wrong answer on 17/19
main :-
    readln([W|Ws],end_of_file),
    string_chars(W,C),
    (pairs_keys_values(V,C,Ws) ->
        (s(V) ->
            T = true
        ;
            T = false
        )
    ;
        T=false
    ),
    writeln(T).

s([]).
s([C-S|Cs]) :-
    ( ((memberchk(C-S2,Cs),S\=S2) ; (memberchk(C2-S,Cs),C\=C2)) ->
        fail
    ;
        s(Cs)
    ).
*/
    

