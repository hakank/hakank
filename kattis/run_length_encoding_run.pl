% https://open.kattis.com/problems/runlengthencodingrun
% 1s
% 1.9 Easy

% H3e2l3o1W1o3r4l2d1!2

main :-
    read_line_to_string(user_input,S),
    split_string(S," ","",[T,Ss]),
    string_chars(Ss,Cs),
    (T =:= 0'E ->
        clumped(Cs,Cl),
        maplist(pe,Cl),
        nl
    ;
        d(Cs,[],R),
        flatten(R,Rs),
        string_chars(Res,Rs),
        writeln(Res)
    ).
pe(X-C) :- format("~w~d",[X,C]).
d([],S,S).
d([Ch,N0|Ns],S0,[Rep|S]) :-
    atom_number(N0,N),
    findall(Ch,between(1,N,_),Rep),
    d(Ns,S0,S).
