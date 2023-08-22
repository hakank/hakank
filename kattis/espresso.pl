% https://open.kattis.com/problems/espresso
% 1s
% 2.1 Easy

main :-
    read_string(user_input,_,S),
    split_string(S,"\n","\n",[NV|Ss]),
    split_string(NV," ","",[_,V0]),
    number_string(V,V0),
    s(Ss,R),
    f(R,V,0,_X,0,F),
    writeln(F).
f([],_,X,X,F,F).
f([N|Ns],V,X0,X,F0,F) :-
    (N+X0 > V ->
        F1 is F0+1,
        X1 is N
    ;
        F1 is F0,
        X1 is X0+N
    ),
    f(Ns,V,X1,X,F1,F).
  
s([],[]).
s([H|T],[X|R]) :-
    string_codes(H,C),
    (length(C,1) ->
        X is C-0'0
    ;
        [N,_]=C,
        X is 1+N-0'0
    ),
    s(T,R).
    

/*
% Compressed: 362 chars. Top 10 is 145..205 chars (including my Python3 program at place 3: 174 chars)
main:-read_string(user_input,_,S),split_string(S,"\n","\n",[NV|Ss]),split_string(NV," ","",[_,V0]),
number_string(V,V0),s(Ss,R),f(R,V,0,_X,0,F),writeln(F).
f([],_,X,X,F,F).
f([N|Ns],V,X0,X,F0,F):-(N+X0>V->F1 is F0+1,X1 is N;F1 is F0,X1 is X0+N),f(Ns,V,X1,X,F1,F).
s([],[]). s([H|T],[X|R]):-string_codes(H,C),(length(C,1)->X is C-0'0;[N,_]=C,X is 1+N-0'0),s(T,R).
*/

