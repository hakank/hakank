% https://open.kattis.com/problems/gandalfsspell
% 1s
% 2.0 Easy

% Testing another approach since using dicts
% requires atoms (which takes some few lines to fix)
% Yes, this is quite shorter and the compressed version got
% in Top 10 (place 5 with 275 chars).

main :-
    read_string(user_input,_,S),
    split_string(S,"\n","\n",[W,T]),
    string_chars(W,Cs),
    split_string(T," ","",Ts),
    (pairs_keys_values(V,Cs,Ts) ->
        (s(V) ->
            R = true
        ;
            R = false
        )
    ;
        R=false
    ),
    writeln(R).
s([]).
s([C-S|Cs]) :-
    ( ((memberchk(C-S2,Cs),S\=S2) ; (memberchk(C2-S,Cs),C\=C2)) ->
        fail
    ;
        s(Cs)
    ).


/* 
% Compressed: 275 chars: Top 10 place 4!
main:-read_string(user_input,_,S),split_string(S,"\n","\n",[W,T]),string_chars(W,Cs),
split_string(T," ","",Ts),(pairs_keys_values(V,Cs,Ts)->(s(V)->R=true;R=false);R=false),writeln(R).
s([]). s([C-S|Cs]):-(((memberchk(C-S2,Cs),S\=S2);(memberchk(C2-S,Cs),C\=C2))->fail;s(Cs)).
*/

