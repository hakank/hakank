% https://open.kattis.com/problems/permutedarithmeticsequence
% 1s
% 2.1 Easy

% Ah, we cannot use readln/2 since it separate "-5" into [-,5]. Bummer!

% Uncompressed: 417 chars
main :-
    read_string(user_input,_,S),
    split_string(S,"\n","\n",[_|T]),
    s(T).
s([]).
s([S|Ss]) :-
    split_string(S," ","",[_|T]),
    maplist(number_string,N,T),
    (t(N)->R=""
    ;
        (msort(N,X),t(X) ->
            R="permuted "
        ;
            R="non-")
    ),
    format("~sarithmetic~n",[R]),
    s(Ss).
t(L) :-
    findall(D,(nextto(A,B,L),D is B-A),E),
    sort(E,V),
    length(V,1).

/*
% Compressed: 304 chars -> Top 10 place 5
main:-read_string(user_input,_,S),split_string(S,"\n","\n",[_|T]),s(T).
s([]). s([S|Ss]):-split_string(S," ","",[_|T]),maplist(number_string,N,T),
(t(N)->R="";(msort(N,X),t(X)->R="permuted ";R="non-")),format("~sarithmetic~n",[R]),s(Ss).
t(L):-findall(D,(nextto(A,B,L),D is B-A),E),sort(E,V),length(V,1).
*/